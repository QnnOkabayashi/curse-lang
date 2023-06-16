use core::{mem::MaybeUninit, slice};

pub mod new;

/// An arena built with eagerly allocating contiguous slices in mind.
///
/// Since this is a wrapper around [`typed_arena::Arena<MaybeUninit<T>>`], this arena
/// does not `Drop` values.
pub struct DroplessArena<T> {
    inner: typed_arena::Arena<MaybeUninit<T>>,
}

impl<T> DroplessArena<T> {
    #[allow(clippy::mut_from_ref)]
    pub fn alloc(&self, value: T) -> &mut T {
        // SAFETY: The value is initialized because of `MaybeUninit::new`.
        unsafe { self.inner.alloc(MaybeUninit::new(value)).assume_init_mut() }
    }

    pub fn alloc_slice(&self, num: usize) -> SliceGuard<'_, T> {
        // SAFETY: The inner arena holds `MaybeUninit<T>`, which is always safe to assume
        // initialized.
        // Also, the `SliceGuard` cannot outlive `self`, so this is safe.
        SliceGuard::new(unsafe {
            &mut *(self.inner.alloc_uninitialized(num) as *mut [MaybeUninit<MaybeUninit<T>>]
                as *mut [MaybeUninit<T>])
        })
    }
}

impl<T> Default for DroplessArena<T> {
    fn default() -> Self {
        DroplessArena {
            inner: Default::default(),
        }
    }
}

pub struct SliceGuard<'arena, T> {
    data: &'arena mut [MaybeUninit<T>],
    len: usize,
}

impl<'arena, T> SliceGuard<'arena, T> {
    pub fn new(data: &'arena mut [MaybeUninit<T>]) -> Self {
        SliceGuard { data, len: 0 }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn capacity(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn is_full(&self) -> bool {
        self.len == self.capacity()
    }

    pub fn push(&mut self, value: T) {
        if self.try_push(value).is_err() {
            panic!("Pushed at full capacity");
        }
    }

    pub fn try_push(&mut self, value: T) -> Result<(), T> {
        let Some(end) = self.data.get_mut(self.len) else {
            return Err(value);
        };

        *end = MaybeUninit::new(value);
        self.len += 1;
        Ok(())
    }

    pub fn extend(mut self, iter: impl IntoIterator<Item = T>) -> &'arena mut [T] {
        for value in iter {
            self.push(value);
        }
        self.into_slice()
    }

    pub fn into_slice(self) -> &'arena mut [T] {
        unsafe {
            slice::from_raw_parts_mut(self.data as *mut [MaybeUninit<T>] as *mut T, self.len())
        }
    }
}
