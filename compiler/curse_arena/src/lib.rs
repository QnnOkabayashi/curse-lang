use std::cell::Cell;
use std::marker::PhantomData;
use std::mem::{size_of, ManuallyDrop};
use std::num::NonZeroUsize;
use std::ptr::NonNull;
use std::{fmt, ops, ptr, slice};

// Initial size in bytes.
// const INITIAL_SIZE: usize = 1024;

// pub struct Arena<T> {
//     chunks: RefCell<ChunkList<T>>,
// }

// struct ChunkList<T> {
//     current: Chunk<T>,
//     remaining: Vec<Chunk<T>>,
// }

// impl<T> Arena<T> {
//     pub fn new() -> Arena<T> {
//         let size = cmp::max(1, mem::size_of::<T>());
//         let cap = INITIAL_SIZE / size;
//         assert!(cap > 0, "Type is too big, use `Arena::with_capacity` instead");
//         Arena::with_capacity(cap)
//     }

//     pub fn with_capacity(capacity: usize) -> Arena<T> {
//         Arena {
//             chunks: RefCell::new(ChunkList {
//                 current: Chunk::with_capacity(capacity),
//                 remaining: Vec::new(),
//             }),
//         }
//     }

//     pub fn len(&self) -> usize {
//         let chunks = self.chunks.borrow();
//         chunks.current.len() + chunks.remaining.iter().map(Chunk::len).sum::<usize>()
//     }

//     pub fn alloc(&self, value: T) -> &T {
//         let mut chunks = self.chunks.borrow_mut();
//         match chunks.current.try_push(value) {
//             Ok(value) => unsafe {
//                 // Extend the lifetime from that of `chunks` to that of `self`.
//                 // This is okay because we're careful to never move items.
//                 mem::transmute::<&T, &T>(value)
//             },
//             Err(value) => {
//                 let mut vec = Vec::with_capacity(chunks.current.len().saturating_mul(2));
//                 vec.push(value);
//                 let old_chunk = mem::replace(&mut chunks.current, Chunk::from_vec(vec));
//                 chunks.remaining.push(old_chunk);

//                 unsafe {
//                     // SAFETY: valid for as long as the arena is valid
//                     chunks.current.ptr.as_ref()
//                 }
//             }
//         }
//     }

//     pub fn for_each(&self, mut f: impl FnMut(&T)) {
//         let chunks = self.chunks.borrow();
//         for chunk in chunks.remaining.iter() {
//             chunk.as_slice().iter().for_each(&mut f);
//         }
//         chunks.current.as_slice().iter().for_each(&mut f);
//     }

//     pub fn map<S>(&self, mut f: impl FnMut(&T) -> S) -> Arena<S> {
//         let mut vec = Vec::with_capacity(self.len());
//         self.for_each(|t| vec.push(f(t)));

//         Arena {
//             chunks: RefCell::new(ChunkList {
//                 current: Chunk::from_vec(vec),
//                 remaining: Vec::new(),
//             }),
//         }
//     }
// }

/// Like `Vec<T>`, but cannot reallocate and allows for pushing via only a
/// shared reference.
///
/// Also, this type derefs to a `&[T]` of the allocated elements, allowing for
/// iteration and manual indexing.
///
/// The main benefit of this type is the [`Chunk::map`] method, which allows
/// for node-based data structures within the `Chunk` to be mapped safely
/// to other data structures, while also preserving edges in the form of `&T`s.
///
/// This will be essential for lowering steps throughout the curse compiler.
pub struct Chunk<T> {
    ptr: NonNull<T>,
    len: Cell<usize>,
    cap: NonZeroUsize,
}

impl<T> Chunk<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        Chunk::from_vec(Vec::with_capacity(capacity))
    }

    pub fn from_vec(vec: Vec<T>) -> Self {
        assert!(size_of::<T>() != 0, "ZSTs are unsupported");
        let cap = NonZeroUsize::new(vec.capacity()).expect("chunks must be larger than 0");
        let mut vec = ManuallyDrop::new(vec);

        unsafe {
            // SAFETY: The ptr that vec uses is guaranteed nonnull.
            Chunk {
                ptr: NonNull::new_unchecked(vec.as_mut_ptr()),
                len: Cell::new(vec.len()),
                cap,
            }
        }
    }

    /// The length of the `Chunk`.
    pub fn len(&self) -> usize {
        self.len.get()
    }

    /// Returns `true` if empty, otherwise `false`.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the total capacity of the [`Chunk`].
    pub fn capacity(&self) -> NonZeroUsize {
        self.cap
    }

    pub fn remaining_capacity(&self) -> usize {
        self.cap.get() - self.len()
    }

    /// Returns a slice of the elements that have been pushed so far.
    pub fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len()) }
    }

    /// Returns a pointer to the next free slot, or `None` if the `Chunk` is full.
    fn next_addr(&self) -> Option<NonNull<T>> {
        if self.len() >= self.cap.get() {
            None
        } else {
            unsafe {
                // SAFETY: `len` is basically len of a vec which is never past
                // `isize::MAX`, it comes from the same allocation, and we just
                // performed a bounds check.
                let ptr = self.ptr.as_ptr().add(self.len());

                // SAFETY: ptr points to within an allocation which cannot
                // be null.
                Some(NonNull::new_unchecked(ptr))
            }
        }
    }

    /// Pushes an element and returns a reference, or gives back the element
    /// if there's not enough space.
    pub fn try_push(&self, value: T) -> Result<&T, T> {
        let Some(end) = self.next_addr() else {
            return Err(value);
        };

        unsafe {
            // SAFETY: same code as `Vec::push` pretty much.
            // https://doc.rust-lang.org/src/alloc/vec/mod.rs.html#1836-1847
            ptr::write(end.as_ptr(), value);
            self.len.set(self.len() + 1);

            // SAFETY: The following has two invariants that must be upheld:
            // 1. We are dereferencing a raw pointer. This is safe because
            //    we just wrote a valid value to the address.
            // 2. We're taking a reference to it, which has an arbitrary lifetime.
            //    This is safe because the lifetime is shortened to the lifetime
            //    of `&self`, which is correct since a `Chunk` never reallocates
            //    its buffer. Thus, the reference will be valid for as long
            //    as the `Chunk` is.
            Ok(&*end.as_ptr())
        }
    }

    /// Returns a fresh [`Chunk`] with the same capacity as `chunk`.
    ///
    /// This is handy for creating a new chunk that you want to map into.
    pub fn new_like<S>(chunk: &Chunk<S>) -> Self {
        Chunk::with_capacity(chunk.cap.get()) // unnecessary `.expect()`, oh well
    }

    pub fn create_ref_map<'a, S>(&'a self, new_chunk: &'a Chunk<S>) -> RefMap<'a, T, S> {
        // assert!(self.len() >= new_chunk.len());
        assert!(size_of::<T>() != 0, "ZSTs are unsupported");

        RefMap {
            allowed_start: self.ptr.as_ptr() as usize / size_of::<T>(),
            initialized: &new_chunk.len,
            new_chunk: new_chunk
                .next_addr()
                .expect("no space in new chunk")
                .as_ptr(),
            _src_marker: PhantomData,
            _dst_marker: PhantomData,
        }
    }

    /// To be used with [`Chunk::new_like`].
    ///
    /// If you `.try_push(...)` on `dst` inside of `f`, whatever you push will
    /// be overwritten and leaked so try not to do that. However, it will still
    /// be safe because [`Ref<'_, T>`] is index-based.
    pub fn map<'chunk, S>(&self, dst: &'chunk Chunk<S>, mut f: impl FnMut(&T) -> S) {
        assert!(
            dst.remaining_capacity() >= self.len(),
            "not enough space to map all elements"
        );

        let initial_len = dst.len();

        for (offset, element) in self.iter().enumerate() {
            unsafe {
                let offset = offset + initial_len;
                // SAFETY: Ensured that `dst.remaining_cap() >= self.len()` holds
                let address = dst.ptr.as_ptr().add(offset);
                // SAFETY: ptr is valid for writes and aligned.
                // If `f` panics, it's okay because `dst.len` hasn't been updated yet
                // so we'll leak whatever's been written thus far.

                // Also, what happens if `f` tries to push to `dst`?
                // It'll just get overwritten and will leak, which is safe.
                ptr::write(address, f(element));

                // Update the length immediately so any `RefMap`s working
                // between the two gets an up-to-date version of the length,
                // (they borrow the `Cell<usize>`).
                // Also: do not depend on `self.len()` because `f` might push
                // something which would update the length. In here, `offset`
                // is the source of truth.
                // Even if they pushed to `self`, it would be fine because we're
                // iterating over a slice which is like a frozen view of `self`.
                dst.len.set(offset + 1);
            }
        }
    }

    // /// Maps a slice into the `Chunk`.
    // /// This is like a very strict version of `.extend(iter)` that allows this
    // /// function to know whether or not it will have enough space beforehand.
    // pub fn fill_slice<S>(&self, slice: &[S], mut f: impl FnMut(&S) -> T) -> Option<&[T]> {
    //     // Ensure that there's space before we start.
    //     if slice.len() > self.cap - self.len() {
    //         return None;
    //     }
    //
    //     unsafe {
    //         let initial_len = self.len.get();
    //         let start: *mut T = self.ptr.as_ptr().add(initial_len);
    //         for (offset, element) in slice.iter().enumerate() {
    //             // SAFETY: ensured above that len + {offset in slice} <= cap
    //             let address = start.add(offset);
    //             // SAFETY: ptr is valid for writes and aligned.
    //             // If `f` panics, it's okay because `self.len` hasn't been updated yet
    //             // so we'll leak whatever's been written thus far.
    //             ptr::write(address, f(element));
    //         }
    //
    //         self.len.set(initial_len + slice.len());
    //         // SAFETY: never mutated and the memory is valid.
    //         Some(std::slice::from_raw_parts(start, slice.len()))
    //     }
    // }
}

impl<T: fmt::Debug> fmt::Debug for Chunk<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_slice(), f)
    }
}

impl<T> ops::Deref for Chunk<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> Drop for Chunk<T> {
    fn drop(&mut self) {
        unsafe {
            let _ = Vec::<T>::from_raw_parts(self.ptr.as_ptr(), self.len.get(), self.cap.get());
        }
    }
}

/// Maps references from the old chunk to the new chunk, ensuring that only
/// valid references are passed in.
///
/// We want to allow our node-based data structures (i.e. trees) to
/// hold plain references to other nodes (`&T`), but in order to map trees into
/// other trees, we have to guarantee a topological sort.
///
/// This type allows for mapping references inside of nodes to the new chunk,
/// while checking that topological ordering is maintained to prevent any UB
/// of holding a reference to uninitialized memory.
pub struct RefMap<'a, Src, Dst> {
    // Pointer to the start of the old `Chunk`, in units of `Src`
    allowed_start: usize,
    // Pointer to the end of the allowed region (exclusive).
    initialized: &'a Cell<usize>,
    // The `Chunk` that is being mapped into.
    new_chunk: *mut Dst,
    _src_marker: PhantomData<&'a Chunk<Src>>,
    _dst_marker: PhantomData<&'a Chunk<Dst>>,
}

impl<'a, Src, Dst> RefMap<'a, Src, Dst> {
    // This function is INCREDIBLY unsafe
    pub fn map(&self, p: &Src) -> &'a Dst {
        // address of ref in units of `Src`
        let offset_from_null = p as *const Src as usize / size_of::<Src>();

        let offset_from_start = offset_from_null
            .checked_sub(self.allowed_start)
            .expect("invalid addr");

        if offset_from_start >= self.initialized.get() {
            panic!("invalid addr");
        }

        unsafe {
            // SAFETY: The address belongs to the chunk and the offset is within
            // the range of initialized values.
            &*self.new_chunk.add(offset_from_start)
        }
    }
}

impl<Src, Dst> Clone for RefMap<'_, Src, Dst> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Src, Dst> Copy for RefMap<'_, Src, Dst> {}

impl<Src, Dst> fmt::Debug for RefMap<'_, Src, Dst> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RefMap").finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    enum IntNode<'a> {
        Int(i32),
        Pair(&'a Self, &'a Self),
    }

    #[derive(Debug)]
    enum StringNode<'a> {
        String(String),
        Pair(&'a Self, &'a Self),
    }

    #[test]
    fn test1() {
        let string_tree: Chunk<StringNode> = Chunk::with_capacity(4);
        let _ = string_tree
            .try_push(StringNode::String("hi".into()))
            .unwrap();
        {
            let int_tree: Chunk<IntNode> = Chunk::with_capacity(3);
            let one: &IntNode<'_> = int_tree.try_push(IntNode::Int(1)).unwrap();
            let two: &IntNode<'_> = int_tree.try_push(IntNode::Int(2)).unwrap();
            let _pair: &IntNode<'_> = int_tree.try_push(IntNode::Pair(one, two)).unwrap();

            println!("{int_tree:?}");

            // We can create the map out here, as it holds a reference to `string_tree`s
            // `len` field (a `Cell<usize>`), which allows it to dynamically check
            // its length which gets updated between calls in [`Chunk::map`]
            let m: RefMap<IntNode, StringNode> = int_tree.create_ref_map(&string_tree);

            int_tree.map(&string_tree, |node: &IntNode| match node {
                IntNode::Int(int32) => StringNode::String(int32.to_string()),
                IntNode::Pair(lhs, rhs) => StringNode::Pair(m.map(lhs), m.map(rhs)),
            });
        }

        println!("{string_tree:?}");
    }
}
