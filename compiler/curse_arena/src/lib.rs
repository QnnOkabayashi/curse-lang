use std::cell::Cell;
use std::mem::ManuallyDrop;
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
/// When pushing an item with [`Chunk::try_push`], a [`Ref<'_, T>`] is returned
/// which allows for safe access into the `Chunk`.
///
/// Also, this type derefs to a `&[T]` of the allocated elements, allowing for
/// iteration and manual indexing.
///
/// The main benefit of this type is the [`Chunk::map`] method, which allows
/// for node-based data structures within the `Chunk` to be mapped safely
/// to a similarly-shaped data structure.
///
/// This will be essential for lowering steps throughout the curse compiler.
pub struct Chunk<T> {
    ptr: NonNull<T>,
    len: Cell<usize>,
    cap: usize,
}

impl<T> Chunk<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        Chunk::from_vec(Vec::with_capacity(capacity))
    }

    pub fn from_vec(vec: Vec<T>) -> Self {
        let mut vec = ManuallyDrop::new(vec);

        unsafe {
            // SAFETY: The ptr that vec uses is guaranteed nonnull.
            Chunk {
                ptr: NonNull::new_unchecked(vec.as_mut_ptr()),
                len: Cell::new(vec.len()),
                cap: vec.capacity(),
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
    pub fn capacity(&self) -> usize {
        self.cap
    }

    pub fn remaining_capacity(&self) -> usize {
        self.cap - self.len()
    }

    /// Returns a slice of the elements that have been pushed so far.
    pub fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len()) }
    }

    /// Pushes an element and returns a reference, or gives back the element
    /// if there's not enough space.
    pub fn try_push(&self, value: T) -> Result<Ref<'_, T>, T> {
        let len = self.len();

        if len == self.cap {
            return Err(value);
        }

        unsafe {
            // SAFETY: We ensured above that there is spare capacity.
            self.ptr.as_ptr().add(len).write(value);
        }

        self.len.set(len + 1);

        Ok(Ref {
            index: len,
            chunk: self,
        })
    }

    /// Returns a fresh [`Chunk`] with the same capacity as `chunk`.
    ///
    /// This is handy for creating a new chunk that you want to map into.
    pub fn new_like<S>(chunk: &Chunk<S>) -> Self {
        Chunk::with_capacity(chunk.cap)
    }

    /// To be used with [`Chunk::new_like`].
    ///
    /// If you `.try_push(...)` on `dst` inside of `f`, whatever you push will
    /// be overwritten and leaked so try not to do that. However, it will still
    /// be safe because [`Ref<'_, T>`] is index-based.
    pub fn map<'chunk, S>(
        &self,
        dst: &'chunk Chunk<S>,
        mut f: impl FnMut(&T, MapRef<'chunk, S>) -> S,
    ) {
        assert!(dst.len() == 0, "chunk must start as empty");
        assert!(
            dst.cap >= self.len(),
            "not enough space to map all elements"
        );

        let into_start: *mut S = dst.ptr.as_ptr();
        for (offset, element) in self.iter().enumerate() {
            unsafe {
                // SAFETY: `dst` has cap >= self.len().
                let address = into_start.add(offset);
                // SAFETY: ptr is valid for writes and aligned.
                // If `f` panics, it's okay because `into.len` hasn't been updated yet
                // so we'll leak whatever's been written thus far.

                // Also, what happens if `f` tries to push to `dst`?
                // It'll just get overwritten and will leak, which is safe.
                // After all, their `Ref` will just index to the wrong thing,
                // but that won't cause any UB.
                ptr::write(address, f(element, MapRef(dst)));
            }
        }

        dst.len.set(self.len());
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
            let _ = Vec::<T>::from_raw_parts(self.ptr.as_ptr(), self.len.get(), self.cap);
        }
    }
}

/// Map a [`Ref<'_, T>`] from one [`Chunk`] into another.
pub struct MapRef<'a, S>(&'a Chunk<S>);

impl<'a, S> MapRef<'a, S> {
    pub fn map<'chunk, T>(self, r: &Ref<'chunk, T>) -> Ref<'a, S> {
        Ref {
            index: r.index,
            chunk: self.0,
        }
    }
}

// Doesn't depend on `S: Clone`, which `derive` automatically inserts.
impl<S> Clone for MapRef<'_, S> {
    fn clone(&self) -> Self {
        MapRef(self.0)
    }
}

// Doesn't depend on `S: Copy`, which `derive` automatically inserts.
impl<S> Copy for MapRef<'_, S> {}

/// An index-based reference into a [`Chunk`].
///
/// This type contains no unsafe code :)
#[derive(Copy, Clone)]
pub struct Ref<'chunk, T> {
    index: usize,
    chunk: &'chunk Chunk<T>,
}

#[test]
fn size_of_ref() {
    println!("{}", std::mem::size_of::<Ref<'_, ()>>());
}

impl<'chunk, T> Ref<'chunk, T> {
    pub fn index(&self) -> usize {
        self.index
    }

    pub fn get(&self) -> &T {
        self.chunk
            .as_slice()
            .get(self.index)
            .expect("uninitialized ref")
    }
}

impl<'chunk, T> ops::Deref for Ref<'chunk, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<T: fmt::Debug> fmt::Debug for Ref<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.get(), f)
    }
}

impl<T: fmt::Display> fmt::Display for Ref<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.get(), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    enum IntTree<'a> {
        Int(i32),
        Pair(Ref<'a, Self>, Ref<'a, Self>),
    }

    #[derive(Debug)]
    enum StringTree<'a> {
        String(String),
        Pair(Ref<'a, Self>, Ref<'a, Self>),
    }

    #[test]
    fn test1() {
        let int_tree: Chunk<IntTree> = Chunk::with_capacity(3);
        let one: Ref<'_, IntTree> = int_tree.try_push(IntTree::Int(1)).unwrap();
        let two: Ref<'_, IntTree> = int_tree.try_push(IntTree::Int(2)).unwrap();
        let _pair: Ref<'_, IntTree> = int_tree.try_push(IntTree::Pair(one, two)).unwrap();

        println!("{int_tree:?}");

        let string_tree: Chunk<StringTree> = Chunk::new_like(&int_tree);
        int_tree.map(
            &string_tree,
            |tree: &IntTree, m: MapRef<StringTree>| match tree {
                IntTree::Int(int32) => StringTree::String(int32.to_string()),
                IntTree::Pair(lhs, rhs) => {
                    // Map the refs to hold a reference to the new tree that's being
                    // mapped into instead. Indices are stable and so they stay
                    // the same.
                    StringTree::Pair(m.map(lhs), m.map(rhs))
                }
            },
        );

        println!("{string_tree:?}");
    }
}

// TODO(quinn): only types get shared across a lot of things.
// Expressions do not.

// So we should use either an index or ptr-based model (but not references)
// so that lowering can be done linearly and elements that point to later
// elements will just point to invalid data until the whole lowering process
// is done. Would probably be smart to do an index-based one and hold a reference
// to the actual graph just in case we do try to early access to ensure safety.
