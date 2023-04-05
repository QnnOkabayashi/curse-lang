use std::cell::Cell;
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
    nonnull: NonNull<T>,
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
                nonnull: NonNull::new_unchecked(vec.as_mut_ptr()),
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
        unsafe { slice::from_raw_parts(self.nonnull.as_ptr(), self.len()) }
    }

    /// Pushes an element and returns a reference, or gives back the element
    /// if there's not enough space.
    pub fn try_push(&self, value: T) -> Result<&T, T> {
        if self.len() >= self.cap.get() {
            return Err(value);
        }

        unsafe {
            // SAFETY: `len` is basically len of a vec which is never past
            // `isize::MAX`, it comes from the same allocation, and we just
            // performed a bounds check.
            let end: *mut T = self.nonnull.as_ptr().add(self.len());

            // SAFETY: same code as `Vec::push` pretty much.
            // https://doc.rust-lang.org/src/alloc/vec/mod.rs.html#1836-1847
            ptr::write(end, value);
            self.len.set(self.len() + 1);

            // SAFETY: The following has two invariants that must be upheld:
            // 1. We are dereferencing a raw pointer. This is safe because
            //    we just wrote a valid value to the address.
            // 2. We're taking a reference to it, which has an arbitrary lifetime.
            //    This is safe because the lifetime is shortened to the lifetime
            //    of `&self`, which is correct since a `Chunk` never reallocates
            //    its buffer. Thus, the reference will be valid for as long
            //    as the `Chunk` is.
            Ok(&*end)
        }
    }

    /// Returns a fresh [`Chunk`] with the same capacity as `chunk`.
    ///
    /// This is handy for creating a new chunk that you want to map into.
    pub fn new_like<S>(chunk: &Chunk<S>) -> Self {
        Chunk::with_capacity(chunk.cap.get()) // unnecessary `.expect()`, oh well
    }

    /// To be used with [`Chunk::new_like`].
    ///
    /// If you `.try_push(...)` on `dst` inside of `f`, whatever you push will
    /// be overwritten and leaked so try not to do that. However, it will still
    /// be safe because [`Ref<'_, T>`] is index-based.
    ///
    /// If you want the index of each node, you can use [`Chunk::index_of`]
    /// on the `&T` passed into your function to get the index.
    pub fn map<'chunk, S>(&self, dst: &'chunk Chunk<S>, mut f: impl FnMut(&T) -> S) {
        assert!(
            dst.remaining_capacity() >= self.len(),
            "not enough space to map all elements"
        );

        for (element, offset) in self.iter().zip(dst.len()..) {
            unsafe {
                // SAFETY: Ensured that `dst.remaining_cap() >= self.len()` holds
                let end = dst.nonnull.as_ptr().add(offset);

                // if `f` pushes to `dst`, then anything they push will get
                // overwritten and leaked.
                // SAFETY: ptr is valid for writes and aligned.
                ptr::write(end, f(element));

                dst.len.set(offset + 1);
            }
        }
    }

    /// Index of the reference in this chunk's array.
    /// This is simply calculated as the difference of two pointers, and is
    /// particularly useful for mapping references in [`Chunk::map`].
    ///
    /// Note that this may return an index _past_ the end of `self`s allocated
    /// data.
    ///
    /// # Panics
    ///
    /// If the reference points to before the start of `self`'s data, then this
    /// method will panic.
    pub fn index_of(&self, reference: &T) -> usize {
        (reference as *const T as usize)
            .checked_sub(self.nonnull.as_ptr() as usize)
            .expect("negative index")
            / size_of::<T>()
    }

    pub fn ref_map<'src, 'dst, Dst>(
        &'src self,
        dst: &'dst Chunk<Dst>,
    ) -> RefMap<'src, 'dst, T, Dst> {
        RefMap {
            src: self,
            dst,
            initial_len: dst.len(),
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

impl<T: fmt::Debug> Chunk<T> {
    /// Pushes an element.
    ///
    /// # Panics
    ///
    /// This method will panic if there's no remaining capacity, as [`Chunk`]
    /// never reallocates.
    pub fn push(&self, value: T) -> &T {
        self.try_push(value).expect("no remaining capacity")
    }
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
            // SAFETY: yes
            let _ = Vec::<T>::from_raw_parts(self.nonnull.as_ptr(), self.len.get(), self.cap.get());
        }
    }
}

/// Helper type for mapping references between maps.
pub struct RefMap<'src, 'dst, Src, Dst> {
    src: &'src Chunk<Src>,
    dst: &'dst Chunk<Dst>,
    initial_len: usize,
}

impl<'src, 'dst, Src, Dst> RefMap<'src, 'dst, Src, Dst> {
    pub fn map(&self, r: &'src Src) -> &'dst Dst {
        &self.dst[self.src.index_of(r) + self.initial_len]
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
        Dummy,
    }

    #[test]
    fn test1() {
        let string_tree: Chunk<StringNode> = Chunk::with_capacity(4);
        let _ = string_tree
            .try_push(StringNode::String("hi".into()))
            .unwrap();

        let int_tree: Chunk<IntNode> = Chunk::with_capacity(3);
        let one: &IntNode<'_> = int_tree.try_push(IntNode::Int(1)).unwrap();
        let two: &IntNode<'_> = int_tree.try_push(IntNode::Int(2)).unwrap();
        let _pair: &IntNode<'_> = int_tree.try_push(IntNode::Pair(one, two)).unwrap();

        let initial_len = string_tree.len();

        int_tree.map(&string_tree, |node: &IntNode| match node {
            IntNode::Int(int32) => StringNode::String(int32.to_string()),
            IntNode::Pair(lhs, rhs) => StringNode::Pair(
                &string_tree[int_tree.index_of(lhs) + initial_len],
                &string_tree[int_tree.index_of(rhs) + initial_len],
            ),
        });

        assert_eq!(
            format!("{int_tree:?}"),
            "[Int(1), Int(2), Pair(Int(1), Int(2))]"
        );
        assert_eq!(
            format!("{string_tree:?}"),
            r#"[String("hi"), String("1"), String("2"), Pair(String("1"), String("2"))]"#
        );
    }

    #[test]
    fn test_push_while_mapping() {
        let string_tree: Chunk<StringNode> = Chunk::with_capacity(4);
        let _ = string_tree
            .try_push(StringNode::String("hi".into()))
            .unwrap();

        let int_tree: Chunk<IntNode> = Chunk::with_capacity(3);
        let one: &IntNode<'_> = int_tree.try_push(IntNode::Int(1)).unwrap();
        let two: &IntNode<'_> = int_tree.try_push(IntNode::Int(2)).unwrap();
        let _pair: &IntNode<'_> = int_tree.try_push(IntNode::Pair(one, two)).unwrap();

        let refmap = int_tree.ref_map(&string_tree);
        int_tree.map(&string_tree, |node: &IntNode| {
            string_tree.push(StringNode::Dummy);
            match node {
                IntNode::Int(int32) => StringNode::String(int32.to_string()),
                IntNode::Pair(lhs, rhs) => StringNode::Pair(refmap.map(lhs), refmap.map(rhs)),
            }
        });

        assert_eq!(
            format!("{int_tree:?}"),
            "[Int(1), Int(2), Pair(Int(1), Int(2))]"
        );

        // no `Dummy` items here, they were all overwritten.
        assert_eq!(
            format!("{string_tree:?}"),
            r#"[String("hi"), String("1"), String("2"), Pair(String("1"), String("2"))]"#
        );
    }
}
