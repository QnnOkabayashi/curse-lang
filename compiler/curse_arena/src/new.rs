use std::ptr::{self, NonNull};
use std::{cell::Cell, fmt, marker::PhantomData, mem::ManuallyDrop, slice};

/// A homogeneous arena type that doesn't drop its values and does not
/// reallocate.
///
/// The benefit is that it can preallocate slices and return them to the user
/// wrapped in a `SliceGuard<'_, T>`, which can then be pushed to (or panic if
/// past capacity). Once the user is done pushing values, they can retrieve
/// access to the underlying `&'arena mut [T]` using `.into_inner()`.
///
/// This allows for easy allocation of recursive `Copy` types that may want to
/// be placed in a slice.
pub struct Arena<T> {
    entries: Cell<usize>,
    capacity: usize,
    ptr: NonNull<T>,
    _marker: PhantomData<T>,
}

pub struct SliceGuard<'arena, T> {
    entries: usize,
    capacity: usize,
    ptr: NonNull<T>,
    _marker: PhantomData<&'arena T>,
}

impl<T> Arena<T> {
    /// Create a new [`Arena`] with a fixed capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        let mut me = ManuallyDrop::new(Vec::with_capacity(capacity));

        // SAFETY: `Vec` points to a valid allocation or is dangling, neither
        // of which can ever be null.
        let ptr = unsafe { NonNull::new_unchecked(me.as_mut_ptr()) };

        Arena {
            entries: Cell::new(0),
            capacity,
            ptr,
            _marker: PhantomData,
        }
    }

    /// Returns the remaining capacity.
    pub fn remaining(&self) -> usize {
        // we use wrapping_sub because it emits less instructions,
        // and it should never wrap since `next` is always <= `storage.len()`
        self.capacity.wrapping_sub(self.entries.get())
    }

    /// Allocates a value in the arena, or returns the value if there's no remaining capacity.
    pub fn try_alloc(&self, value: T) -> Result<&mut T, T> {
        let index = self.entries.get();

        if index == self.capacity {
            return Err(value);
        }

        unsafe {
            let ptr = self.ptr.as_ptr().add(index);

            ptr::write(ptr, value);

            self.entries.set(self.entries.get().wrapping_add(1));

            // SAFETY: each spot in the array is only aliased on creation here,
            // which means this is the only unique reference.
            Ok(&mut *ptr)
        }
    }

    pub fn try_prealloc_slice(&self, capacity: usize) -> Result<SliceGuard<'_, T>, ()> {
        let remaining = self.remaining();

        if capacity > remaining {
            return Err(());
        }

        let index = self.entries.get();

        let ptr = unsafe {
            let ptr = self.ptr.as_ptr().add(index);
            NonNull::new_unchecked(ptr)
        };

        self.entries.set(index.wrapping_add(capacity));

        Ok(SliceGuard {
            entries: 0,
            capacity,
            ptr,
            _marker: PhantomData,
        })
    }
}

impl<T> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Arena")
            .field("entries", &self.entries)
            .finish_non_exhaustive()
    }
}

impl<T> Drop for Arena<T> {
    /// the `Drop` impl just frees the allocated memory without running
    /// destructors on the allocated values. We do this because not all the
    /// allocated values are necessarily initialized.
    fn drop(&mut self) {
        unsafe {
            let _: Vec<T> = Vec::from_raw_parts(self.ptr.as_ptr(), 0, self.capacity);
        }
    }
}

// `Send` is okay but `Sync` is not because we use a `Cell` to track the next position.
unsafe impl<T: Send> Send for Arena<T> {}

impl<'arena, T> SliceGuard<'arena, T> {
    pub fn push(&mut self, value: T) {
        if self.try_push(value).is_err() {
            panic!("Pushed at full capacity");
        }
    }

    pub fn try_push(&mut self, value: T) -> Result<(), T> {
        if self.entries == self.capacity {
            return Err(value);
        }

        unsafe {
            let ptr = self.ptr.as_ptr().add(self.entries);
            ptr::write(ptr, value);
        };

        self.entries += 1;
        Ok(())
    }

    pub fn extend<I: Iterator<Item = T>>(mut self, iter: I) -> &'arena mut [T] {
        for value in iter {
            self.push(value);
        }

        self.into_slice()
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.entries) }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.entries) }
    }

    pub fn into_slice(self) -> &'arena mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.entries) }
    }
}

impl<T: fmt::Debug> fmt::Debug for SliceGuard<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_slice(), f)
    }
}

unsafe impl<T: Send> Send for SliceGuard<'_, T> {}
unsafe impl<T: Sync> Sync for SliceGuard<'_, T> {}

#[test]
fn test_safety() {
    let arena = Arena::with_capacity(10);

    let four = arena.try_alloc(0).unwrap();
    let slice = arena.try_prealloc_slice(9).unwrap().extend(1..10);

    println!("{four} and {slice:?}");
}
