use std::{
    cmp,
    ffi::{c_char, c_void},
    fmt::Debug,
    marker::PhantomData,
    mem::{MaybeUninit, transmute},
    num::NonZero,
    ops::{Deref, DerefMut, Range, RangeInclusive},
    ptr::{NonNull, without_provenance, without_provenance_mut},
};

use snapshot_derive::{DeserializeEngine, SerializeEngine};

use crate::{Mut, Ref};

#[derive(SerializeEngine, DeserializeEngine)]
#[repr(C)]
pub struct EcsVec<T> {
    backing_store: Option<BackingStore<T>>,
    len: u32,
    marker: PhantomData<T>,
}

impl<T> Default for EcsVec<T> {
    fn default() -> Self {
        Self {
            backing_store: None,
            len: 0,
            marker: PhantomData,
        }
    }
}

impl<T> EcsVec<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let mut vec = Self::new();

        if size_of::<T>() > 0 {
            let cap = u32::try_from(capacity)
                .expect("EcsVec::with_capacity(): Capacity overflowed u32::MAX.");
            vec.grow_backing_store_amortized(0, cap);
        }

        vec
    }

    pub fn capacity(&self) -> usize {
        if size_of::<T>() == 0 {
            u32::MAX as usize
        } else {
            self.backing_store
                .as_ref()
                .map_or(0, |store| store.info().capacity::<T>() as usize)
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    /// # Safety
    ///
    /// - `new_len` must be less than or equal to [`capacity()`].
    /// - The elements at `old_len..new_len` must be initialized.
    #[inline]
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());

        self.len = new_len as u32;
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn get<I>(&self, index: I) -> Option<<I as EcsSliceIndex<T>>::OutputRef<'_>>
    where
        I: EcsSliceIndex<T>,
    {
        index.get(self)
    }

    pub fn get_mut<I>(&mut self, index: I) -> Option<<I as EcsSliceIndex<T>>::OutputMut<'_>>
    where
        I: EcsSliceIndex<T>,
    {
        index.get_mut(self)
    }

    pub fn push(&mut self, value: T) {
        // Inform codegen that the length does not change across grow().
        let len = self.len;

        // Call `BackingStore::info()` as few times as possible, as it requires
        // an FFI call.
        let (ptr, cap) = self.backing_store.as_ref().map_or_else(
            || {
                let cap = if size_of::<T>() == 0 { u32::MAX } else { 0 };
                (NonNull::dangling(), cap)
            },
            |backing_store| {
                let info = backing_store.info();
                (info.ptr, info.capacity::<T>())
            },
        );

        let ptr = if len == cap {
            self.grow_backing_store_amortized(cap, 1);

            // `ptr` will have changed with the grow, fetch again.
            // `backing_store` is guaranteed to exist after growing.
            self.backing_store.as_ref().unwrap().info().ptr
        } else {
            ptr
        };

        unsafe {
            let end = ptr.cast::<MaybeUninit<T>>().add(len as usize);
            Mut::new(end).write(value);
            self.len = len + 1;
        }
    }

    fn grow_backing_store_amortized(&mut self, cap: u32, additional: u32) {
        if size_of::<T>() == 0 {
            // There is no backing store when `T` is zero-sized.
            unreachable!("EcsVec::grow_amortized(): Tried to grow capacity with zero-sized type.");
        }

        if let Some(store) = &mut self.backing_store {
            let required_cap = cap
                .checked_add(additional)
                .expect("EcsVec::grow_amortized(): Capacity overflow.");

            // This guarantees exponential growth.
            let cap = cmp::max(cap.saturating_mul(2), required_cap);

            store.realloc(cap);
        } else {
            // If `T` is small, allocate capacity for multiple.
            let min_non_zero_cap = if size_of::<T>() == 1 {
                8
            } else if size_of::<T>() <= 1024 {
                4
            } else {
                1
            };

            let cap = cmp::max(additional, min_non_zero_cap);

            self.backing_store = BackingStore::with_capacity(cap).into();
        }
    }

    /// Removes and returns the element at position `index` within the vector,
    /// shifting all elements after it to the left.
    ///
    /// TODO: also implement for `dynamic_wasm` feature.
    #[cfg(not(feature = "dynamic_wasm"))]
    #[track_caller]
    pub fn remove(&mut self, index: usize) -> T {
        #[cold]
        #[track_caller]
        fn assert_failed(index: usize, len: usize) -> ! {
            panic!("removal index (is {index}) should be < len (is {len})");
        }

        let len = self.len();
        if index >= len {
            assert_failed(index, len);
        }

        unsafe {
            // Infallible.
            let ret;

            {
                // The place we are taking from.
                let ptr = self.as_ptr().add(index).as_ptr();
                // Copy it out, unsafely having a copy of the value on the stack
                // and in the vector at the same time.
                ret = std::ptr::read(ptr);

                // Shift everything down to fill in that spot.
                std::ptr::copy(ptr.add(1), ptr, len - index - 1);
            }

            self.set_len(len - 1);
            ret
        }
    }

    fn as_ptr(&self) -> NonNull<T> {
        self.backing_store
            .as_ref()
            .map_or(NonNull::dangling(), |store| store.info().ptr)
            .cast::<T>()
    }

    /// # Performance
    ///
    /// When compiling with the `dynamic_wasm` feature, this will copy the
    /// entire slice to Wasm memory. If the entire slice is not needed, it is
    /// preferred to use `EcsVec::get()`, which only copies the requested range.
    ///
    /// There is no performance penalty when compiling without the
    /// `dynamic_wasm` feature.
    pub fn as_slice(&self) -> EcsSliceRef<'_, T> {
        unsafe { EcsSliceRef::new(self.as_ptr(), self.len) }
    }

    /// # Performance
    ///
    /// When compiling with the `dynamic_wasm` feature, this will copy the
    /// entire slice to Wasm memory. If the entire slice is not needed, it is
    /// preferred to use `EcsVec::get_mut()`, which only copies the requested
    /// range.
    ///
    /// There is no performance penalty when compiling without the
    /// `dynamic_wasm` feature.
    pub fn as_mut_slice(&mut self) -> EcsSliceMut<'_, T> {
        unsafe { EcsSliceMut::new(self.as_ptr(), self.len) }
    }

    pub fn iter(&self) -> EcsSliceIter<'_, T> {
        EcsSliceIter::new(self)
    }

    pub fn iter_mut(&mut self) -> EcsSliceIterMut<'_, T> {
        EcsSliceIterMut::new(self)
    }

    /// Creates an `EcsVec<T>` directly from a backing store and a length.
    ///
    /// # Safety
    ///
    /// The backing store and the length **must** have been taken from
    /// `into_raw_parts` and must not have been modified or mismatched.
    pub unsafe fn from_raw_parts(backing_store: Option<BackingStore<T>>, len: u32) -> Self {
        Self {
            backing_store,
            len,
            marker: PhantomData,
        }
    }

    /// Decomposes an `EcsVec<T>` into its raw components: `(backing store,
    /// length)`.
    pub fn into_raw_parts(self) -> (Option<BackingStore<T>>, u32) {
        (self.backing_store, self.len)
    }
}

impl<T: Debug> Debug for EcsVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl<T, const N: usize> From<[T; N]> for EcsVec<T> {
    fn from(value: [T; N]) -> Self {
        value.into_iter().collect()
    }
}

impl<T: Clone> From<&[T]> for EcsVec<T> {
    fn from(value: &[T]) -> Self {
        value.iter().cloned().collect()
    }
}

impl<T> FromIterator<T> for EcsVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (lower, _) = iter.size_hint();
        let mut vec = Self::with_capacity(lower);

        for elem in iter {
            vec.push(elem);
        }

        vec
    }
}

impl<'a, T> IntoIterator for &'a EcsVec<T> {
    type Item = Ref<'a, T>;

    type IntoIter = EcsSliceIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut EcsVec<T> {
    type Item = Mut<'a, T>;

    type IntoIter = EcsSliceIterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

pub struct EcsSliceRef<'a, T> {
    #[cfg(not(feature = "dynamic_wasm"))]
    slice: &'a [T],

    /// We need to copy external memory to the Wasm instance's memory, but we do
    /// not own this data! We wrap the values in `ManuallyDrop` to prevent the
    /// compiler from dropping the values.
    #[cfg(feature = "dynamic_wasm")]
    data: Vec<std::mem::ManuallyDrop<T>>,

    marker: PhantomData<&'a [T]>,
}

impl<T> EcsSliceRef<'_, T> {
    unsafe fn new(ptr: NonNull<T>, len: u32) -> Self {
        let len = len as usize;

        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            Self {
                slice: std::slice::from_raw_parts(ptr.as_ptr(), len),
                marker: PhantomData,
            }
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            let mut data = Vec::<std::mem::ManuallyDrop<T>>::with_capacity(len);
            crate::wasm::wasm_read_external_data(
                data.as_mut_ptr().cast(),
                ptr.as_ptr().cast(),
                size_of::<T>() * len,
            );
            data.set_len(len);

            Self {
                data,
                marker: PhantomData,
            }
        }
    }
}

impl<T> AsRef<[T]> for EcsSliceRef<'_, T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T> Deref for EcsSliceRef<'_, T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        #[cfg(not(feature = "dynamic_wasm"))]
        {
            self.slice
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            // Safety: `ManuallyDrop<T>` is guaranteed to have the same layout
            // as `T`.
            transmute::<&[std::mem::ManuallyDrop<T>], &[T]>(self.data.as_slice())
        }
    }
}

pub struct EcsSliceMut<'a, T> {
    ptr: NonNull<T>,

    #[cfg(not(feature = "dynamic_wasm"))]
    len: usize,

    /// We need to copy external memory to the Wasm instance's memory, but we do
    /// not own this data! We wrap the values in `ManuallyDrop` to prevent the
    /// compiler from dropping the values.
    #[cfg(feature = "dynamic_wasm")]
    data: Vec<std::mem::ManuallyDrop<T>>,

    marker: PhantomData<&'a mut [T]>,
}

impl<T> EcsSliceMut<'_, T> {
    unsafe fn new(ptr: NonNull<T>, len: u32) -> Self {
        let len = len as usize;

        #[cfg(not(feature = "dynamic_wasm"))]
        {
            Self {
                ptr,
                len,
                marker: PhantomData,
            }
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            let mut data = Vec::<std::mem::ManuallyDrop<T>>::with_capacity(len);
            crate::wasm::wasm_read_external_data(
                data.as_mut_ptr().cast(),
                ptr.as_ptr().cast(),
                size_of::<T>() * len,
            );
            data.set_len(len);

            Self {
                ptr,
                data,
                marker: PhantomData,
            }
        }
    }
}

impl<T> Deref for EcsSliceMut<'_, T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            std::slice::from_raw_parts(self.ptr.as_ptr(), self.len)
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            // Safety: `ManuallyDrop<T>` is guaranteed to have the same layout
            // as `T`.
            transmute::<&[std::mem::ManuallyDrop<T>], &[T]>(self.data.as_slice())
        }
    }
}

impl<'a, T> DerefMut for EcsSliceMut<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len)
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            // Safety: `ManuallyDrop<T>` is guaranteed to have the same layout
            // as `T`.
            transmute::<&mut [std::mem::ManuallyDrop<T>], &mut [T]>(self.data.as_mut_slice())
        }
    }
}

#[cfg(feature = "dynamic_wasm")]
impl<T> Drop for EcsSliceMut<'_, T> {
    fn drop(&mut self) {
        unsafe {
            crate::wasm::wasm_write_external_data(
                self.ptr.as_ptr().cast(),
                self.data.as_ptr().cast(),
                self.data.len() * size_of::<T>(),
            );
        }
    }
}

pub trait EcsSliceIndex<T> {
    type OutputRef<'a>
    where
        T: 'a;

    type OutputMut<'a>
    where
        T: 'a;

    fn get(self, vec: &EcsVec<T>) -> Option<Self::OutputRef<'_>>;

    fn get_mut(self, vec: &mut EcsVec<T>) -> Option<Self::OutputMut<'_>>;
}

impl<T> EcsSliceIndex<T> for usize {
    type OutputRef<'a>
        = Ref<'a, T>
    where
        T: 'a;

    type OutputMut<'a>
        = Mut<'a, T>
    where
        T: 'a;

    fn get(self, vec: &EcsVec<T>) -> Option<Self::OutputRef<'_>> {
        if self >= vec.len as usize {
            return None;
        }

        Some(unsafe { Ref::new(vec.as_ptr().add(self)) })
    }

    fn get_mut(self, vec: &mut EcsVec<T>) -> Option<Self::OutputMut<'_>> {
        if self >= vec.len as usize {
            return None;
        }

        Some(unsafe { Mut::new(vec.as_ptr().add(self)) })
    }
}

impl<T> EcsSliceIndex<T> for Range<usize> {
    type OutputRef<'a>
        = EcsSliceRef<'a, T>
    where
        T: 'a;

    type OutputMut<'a>
        = EcsSliceMut<'a, T>
    where
        T: 'a;

    #[inline]
    fn get<'a>(self, vec: &'a EcsVec<T>) -> Option<Self::OutputRef<'a>> {
        // Using checked_sub is a safe way to get `SubUnchecked` in MIR.
        if let Some(new_len) = usize::checked_sub(self.end, self.start)
            && self.end <= vec.len as usize
        {
            // SAFETY: `self` is checked to be valid and in bounds above.
            unsafe { Some(EcsSliceRef::new(vec.as_ptr(), new_len as u32)) }
        } else {
            None
        }
    }

    #[inline]
    fn get_mut(self, vec: &mut EcsVec<T>) -> Option<Self::OutputMut<'_>> {
        if let Some(new_len) = usize::checked_sub(self.end, self.start)
            && self.end <= vec.len as usize
        {
            // SAFETY: `self` is checked to be valid and in bounds above.
            unsafe { Some(EcsSliceMut::new(vec.as_ptr(), new_len as u32)) }
        } else {
            None
        }
    }
}

impl<T> EcsSliceIndex<T> for RangeInclusive<usize> {
    type OutputRef<'a>
        = EcsSliceRef<'a, T>
    where
        T: 'a;

    type OutputMut<'a>
        = EcsSliceMut<'a, T>
    where
        T: 'a;

    #[inline]
    fn get<'a>(self, vec: &'a EcsVec<T>) -> Option<Self::OutputRef<'a>> {
        if *self.end() == usize::MAX {
            None
        } else {
            (*self.start()..*self.end() + 1).get(vec)
        }
    }

    #[inline]
    fn get_mut(self, vec: &mut EcsVec<T>) -> Option<Self::OutputMut<'_>> {
        if *self.end() == usize::MAX {
            None
        } else {
            (*self.start()..*self.end() + 1).get_mut(vec)
        }
    }
}

pub struct EcsSliceIter<'a, T> {
    /// The pointer to the next element to return, or the past-the-end location
    /// if the iterator is empty.
    ///
    /// This address will be used for all ZST elements, never changed.
    ptr: NonNull<T>,
    /// For non-ZSTs, the non-null pointer to the past-the-end element.
    ///
    /// For ZSTs, this is `ptr::without_provenance_mut(len)`.
    end_or_len: *const T,

    marker: PhantomData<&'a [T]>,
}

impl<'a, T> EcsSliceIter<'a, T> {
    fn new(vec: &'a EcsVec<T>) -> Self {
        let len = vec.len as usize;
        let ptr = vec.as_ptr();
        unsafe {
            let end_or_len = if size_of::<T>() == 0 {
                without_provenance(len)
            } else {
                ptr.as_ptr().add(len)
            };

            Self {
                ptr,
                end_or_len,
                marker: PhantomData,
            }
        }
    }
}

impl<'a, T> Iterator for EcsSliceIter<'a, T> {
    type Item = Ref<'a, T>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let ptr = self.ptr;
        let end_or_len = self.end_or_len;

        unsafe {
            if size_of::<T>() == 0 {
                let len = end_or_len.addr();
                if len == 0 {
                    return None;
                }
                self.end_or_len = without_provenance_mut(len.unchecked_sub(1));
            } else {
                if ptr == transmute::<*const T, NonNull<T>>(end_or_len) {
                    return None;
                }
                self.ptr = ptr.add(1);
            }

            Some(Ref::new(ptr))
        }
    }
}

pub struct EcsSliceIterMut<'a, T> {
    /// The pointer to the next element to return, or the past-the-end location
    /// if the iterator is empty.
    ///
    /// This address will be used for all ZST elements, never changed.
    ptr: NonNull<T>,
    /// For non-ZSTs, the non-null pointer to the past-the-end element.
    ///
    /// For ZSTs, this is `ptr::without_provenance_mut(len)`.
    end_or_len: *const T,

    marker: PhantomData<&'a mut [T]>,
}

impl<'a, T> EcsSliceIterMut<'a, T> {
    fn new(vec: &'a mut EcsVec<T>) -> Self {
        let len = vec.len as usize;
        let ptr = vec.as_ptr();
        unsafe {
            let end_or_len = if size_of::<T>() == 0 {
                without_provenance(len)
            } else {
                ptr.as_ptr().add(len)
            };

            Self {
                ptr,
                end_or_len,
                marker: PhantomData,
            }
        }
    }
}

impl<'a, T> Iterator for EcsSliceIterMut<'a, T> {
    type Item = Mut<'a, T>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let ptr = self.ptr;
        let end_or_len = self.end_or_len;

        unsafe {
            if size_of::<T>() == 0 {
                let len = end_or_len.addr();
                if len == 0 {
                    return None;
                }
                self.end_or_len = without_provenance_mut(len.unchecked_sub(1));
            } else {
                if ptr == transmute::<*const T, NonNull<T>>(end_or_len) {
                    return None;
                }
                self.ptr = ptr.add(1);
            }

            Some(Mut::new(ptr))
        }
    }
}

#[derive(SerializeEngine, DeserializeEngine)]
#[repr(transparent)]
pub struct BackingStore<T> {
    handle: BackingStoreHandle,
    marker: PhantomData<T>,
}

pub type BackingStoreHandle = NonZero<u32>;

/// Type-erased FFI struct to retrieve information about a backing store from
/// the engine. `size` is in bytes, not in the number of sized entries.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct BackingStoreInfo {
    pub ptr: NonNull<c_void>,
    pub size: u32,
    pub align: u32,
}

impl<T> BackingStore<T> {
    fn with_capacity(capacity: u32) -> Self {
        let size = size_of::<T>() as u32 * capacity;
        let align = align_of::<T>() as u32;

        let handle = unsafe { BACKING_STORE_ALLOC.unwrap_unchecked()(size, align) };

        Self {
            handle,
            marker: PhantomData,
        }
    }

    fn realloc(&mut self, new_capacity: u32) {
        let new_size = size_of::<T>() as u32 * new_capacity;

        unsafe { BACKING_STORE_REALLOC.unwrap_unchecked()(self.handle, new_size) }
    }

    fn info(&self) -> BackingStoreInfo {
        let mut ret = MaybeUninit::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe { BACKING_STORE_INFO.unwrap_unchecked()(self.handle, &mut ret) };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_uninit_and_read_external(&mut ret, |ret| {
                BACKING_STORE_INFO.unwrap_unchecked()(self.handle, ret)
            })
        };

        assert!(res, "Invalid backing store.");

        unsafe { ret.assume_init() }
    }
}

impl BackingStoreInfo {
    /// Converts from backing store byte size to entry capacity.
    fn capacity<T>(&self) -> u32 {
        self.size / size_of::<T>() as u32
    }
}

impl<T> Drop for BackingStore<T> {
    fn drop(&mut self) {
        unsafe { BACKING_STORE_DEALLOC.unwrap_unchecked()(self.handle) }
    }
}

static mut BACKING_STORE_ALLOC: Option<
    unsafe extern "C" fn(size: u32, align: u32) -> BackingStoreHandle,
> = None;

static mut BACKING_STORE_REALLOC: Option<
    unsafe extern "C" fn(handle: BackingStoreHandle, size: u32),
> = None;

static mut BACKING_STORE_DEALLOC: Option<unsafe extern "C" fn(handle: BackingStoreHandle)> = None;

static mut BACKING_STORE_INFO: Option<
    unsafe extern "C" fn(handle: BackingStoreHandle, *mut MaybeUninit<BackingStoreInfo>) -> bool,
> = None;

#[allow(clippy::missing_transmute_annotations, clippy::missing_safety_doc)]
pub unsafe fn load_proc_addrs(
    get_proc_addr: unsafe extern "C" fn(*const c_char, *mut c_void) -> Option<NonNull<c_void>>,
    ctx: *mut c_void,
) {
    unsafe {
        BACKING_STORE_ALLOC = transmute(get_proc_addr(c"dst::backing_store_alloc".as_ptr(), ctx));
        BACKING_STORE_REALLOC =
            transmute(get_proc_addr(c"dst::backing_store_realloc".as_ptr(), ctx));
        BACKING_STORE_DEALLOC =
            transmute(get_proc_addr(c"dst::backing_store_dealloc".as_ptr(), ctx));
        BACKING_STORE_INFO = transmute(get_proc_addr(c"dst::backing_store_info".as_ptr(), ctx));
    }
}
