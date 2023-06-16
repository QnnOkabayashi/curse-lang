use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr::{self, NonNull};
use std::{cell::Cell, fmt, slice};

/// A homogeneous arena type that doesn't drop its values and does not reallocate.
///
/// The benefit is that it can preallocate slices and return them to the user wrapped in a `SliceGuard<'_, T>`, which can then be pushed to (or panic if past capacity). Once the user is done pushing values, they can retrieve access to the underlying `&'arena mut [T]` using `.into_inner()`.
///
/// This allows for easy allocation of recursive `Copy` types that may want to be placed in a slice.
pub struct Arena<T> {
    entries: Cell<usize>,
    storage: NonNull<[MaybeUninit<T>]>,
}

pub struct SliceGuard<'arena, T> {
    entries: usize,
    slice: &'arena mut [MaybeUninit<T>],
}

#[derive(Debug)]
pub struct PreallocError {
    pub remaining_cap: usize,
    pub requested_cap: usize,
}

impl<T> Arena<T> {
    /// Create a new [`Arena`] with a fixed capacity.
    pub fn with_capacity(cap: usize) -> Self {
        let mut me = ManuallyDrop::new(Vec::with_capacity(cap));

        // SAFETY: The memory is initialized because it's elements, `MaybeUninit<T>`,
        // are considered initialized even on uninitialized memory.
        let slice: &mut [MaybeUninit<T>] =
            unsafe { slice::from_raw_parts_mut(me.as_mut_ptr(), cap) };

        Arena {
            entries: Cell::new(0),
            storage: NonNull::from(slice),
        }
    }

    /// Returns the remaining capacity.
    pub fn remaining(&self) -> usize {
        // we use wrapping_sub because it emits less instructions,
        // and it should never wrap since `next` is always <= `storage.len()`
        self.storage.len().wrapping_sub(self.entries.get())
    }

    /// Allocates a value in the arena, or returns the value if there's no remaining capacity.
    pub fn try_alloc(&self, value: T) -> Result<&mut T, T> {
        let next = self.entries.get();

        if next == self.storage.len() {
            return Err(value);
        }

        unsafe {
            // SAFETY: just checked that index is in range of the allocation
            let ptr = self.storage.cast::<T>().as_ptr().add(next);

            ptr::write(ptr, value);

            self.entries.set(next + 1);

            // SAFETY: each spot in the array is only aliased on creation here,
            // which means this is the only unique reference.
            Ok(&mut *ptr)
        }
    }

    pub fn try_prealloc_slice(&self, cap: usize) -> Result<SliceGuard<'_, T>, PreallocError> {
        // this will never wrap, but allows us to omit overflow checking.
        let remaining = self.remaining();
        if cap > remaining {
            return Err(PreallocError {
                remaining_cap: remaining,
                requested_cap: cap,
            });
        }

        let next = self.entries.get();

        // SAFETY: each spot in the slice is only aliased on creation here,
        // which means this is the only unique reference.
        let slice = unsafe {
            let ptr = self.storage.cast::<MaybeUninit<_>>().as_ptr().add(next);
            slice::from_raw_parts_mut(ptr, cap)
        };

        // use wrapping for the same reason as in `Arena::remaining`
        self.entries.set(next.wrapping_add(cap));

        Ok(SliceGuard { slice, entries: 0 })
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
    /// the `Drop` impl just frees the allocated memory without running destructors
    /// on the allocated values.
    fn drop(&mut self) {
        let capacity = self.storage.len();
        let ptr = self.storage.cast::<MaybeUninit<T>>().as_ptr();

        unsafe {
            let _: Vec<MaybeUninit<T>> = Vec::from_raw_parts(ptr, 0, capacity);
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
        let Some(end) = self.slice.get_mut(self.entries) else {
            return Err(value);
        };

        *end = MaybeUninit::new(value);
        self.entries += 1;
        Ok(())
    }

    pub fn extend<I: Iterator<Item = T>>(mut self, iter: I) -> &'arena mut [T] {
        for value in iter {
            self.push(value);
        }

        self.into_inner()
    }

    pub fn into_inner(self) -> &'arena mut [T] {
        unsafe {
            slice::from_raw_parts_mut(self.slice as *mut [MaybeUninit<T>] as *mut T, self.entries)
        }
    }
}

impl<T> fmt::Debug for SliceGuard<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SliceGuard")
            .field("entries", &self.entries)
            .finish_non_exhaustive()
    }
}

impl fmt::Display for PreallocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "requested capacity for {} values, but the remaining capacity was {}",
            self.requested_cap, self.remaining_cap
        )
    }
}

impl std::error::Error for PreallocError {}

#[test]
fn test_safety() {
    let arena = Arena::with_capacity(10);

    let four = arena.try_alloc(0).unwrap();
    let slice = arena.try_prealloc_slice(9).unwrap().extend(1..10);

    println!("{four} and {slice:?}");
}
