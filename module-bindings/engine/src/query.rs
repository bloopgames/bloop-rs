use std::{
    error::Error,
    ffi::{c_int, c_void},
    marker::PhantomData,
    mem::MaybeUninit,
    ptr::NonNull,
};

use engine_derive::all_tuples_enumerated_with_size;

use crate::{Component, EntityId, FfiStr, Mut, Ref};

/// A query is essentially an iterator over a number of entities, based on the specified
/// template components. For example, a query of type `Query<&Transform>` will iterate over
/// all the entities with a Transform component, and provide access to their `Transform` component.
///
/// Generic `Q` specifies the components to include in this query. Components *must* be references.
/// If the query specifies more than one component, `Q` should be a tuple (i.e. `Query<(&A, &B)>`).
#[repr(C)]
pub struct Query<'a, Q: QueryData> {
    handle: *mut c_void,
    marker: PhantomData<&'a Q>,
}

unsafe impl<Q: QueryData> Send for Query<'_, Q> {}
unsafe impl<Q: QueryData> Sync for Query<'_, Q> {}

impl<'a, Q: QueryData> Query<'a, Q> {
    /// # Safety
    ///
    /// `Query` should only be constructed from a valid pointer retrieved from a
    /// corresponding `Query` parameter in an ECS system's FFI function.
    pub unsafe fn new(query_handle: *mut c_void) -> Self {
        Self {
            handle: query_handle,
            marker: PhantomData,
        }
    }

    /// Returns the number of entities matched by this query.
    pub fn len(&self) -> usize {
        unsafe { _LEN_FN.unwrap_unchecked()(self.handle) }
    }

    /// Returns whether this query does not match any entities.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an immutable reference to a set of components in this query.
    ///
    /// `index` is the index in this query to look up.
    ///
    /// Returns `None` if the lookup failed (i.e. the index is out of bounds).
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(query: Query<(&mut Transform, &Color)>) {
    ///     if let Some(components) = query.get(0) {
    ///         let (transform, color) = components.unpack();
    ///     }
    /// }
    /// ```
    pub fn get(&self, index: usize) -> Option<Q::ReadOnly<'a>> {
        let mut component_ptrs = MaybeUninit::<Q::DataPtrs>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe {
            _GET_FN.unwrap_unchecked()(
                self.handle,
                index,
                (&mut component_ptrs as *mut MaybeUninit<Q::DataPtrs>).cast(),
            )
        };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_uninit_and_read_external(&mut component_ptrs, |ptr| {
                _GET_FN.unwrap_unchecked()(self.handle, index, ptr.cast())
            })
        };

        if res == 0 {
            Some(unsafe { Q::new_read_only(component_ptrs.assume_init()) })
        } else {
            None
        }
    }

    /// Returns a mutable reference to a set of components in this query.
    ///
    /// `index` is the index in this query to look up.
    ///
    /// Returns `None` if the lookup failed (i.e. the index is out of bounds).
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(mut query: Query<(&mut Transform, &Color)>) {
    ///     if let Some(mut components) = query.get_mut(0) {
    ///         let (transform, color) = components.unpack();
    ///     }
    /// }
    /// ```
    pub fn get_mut(&mut self, index: usize) -> Option<Q::Item<'a>> {
        let mut component_ptrs = MaybeUninit::<Q::DataPtrs>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe {
            _GET_FN.unwrap_unchecked()(
                self.handle,
                index,
                (&mut component_ptrs as *mut MaybeUninit<Q::DataPtrs>).cast(),
            )
        };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_uninit_and_read_external(&mut component_ptrs, |ptr| {
                _GET_FN.unwrap_unchecked()(self.handle, index, ptr.cast())
            })
        };

        if res == 0 {
            Some(unsafe { Q::new_item(component_ptrs.assume_init()) })
        } else {
            None
        }
    }

    /// Returns an immutable reference to a set of components in this query.
    ///
    /// `entity_id` is the entity in this query to look up.
    ///
    /// Returns `None` if the lookup failed (i.e. the entity does not exist in this query).
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(query: Query<(&mut Transform, &Color)>) {
    ///     if let Some(components) = query.get_entity(entity_id) {
    ///         let (transform, color) = components.unpack();
    ///     }
    /// }
    /// ```
    pub fn get_entity(&self, entity_id: EntityId) -> Option<Q::ReadOnly<'a>> {
        let mut component_ptrs = MaybeUninit::<Q::DataPtrs>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe {
            _GET_ENTITY_FN.unwrap_unchecked()(
                self.handle,
                entity_id,
                (&mut component_ptrs as *mut MaybeUninit<Q::DataPtrs>).cast(),
            )
        };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_uninit_and_read_external(&mut component_ptrs, |ptr| {
                _GET_ENTITY_FN.unwrap_unchecked()(self.handle, entity_id, ptr.cast())
            })
        };

        if res == 0 {
            Some(unsafe { Q::new_read_only(component_ptrs.assume_init()) })
        } else {
            None
        }
    }

    /// Returns a mutable reference to a set of components in this query.
    ///
    /// `entity_id` is the entity in this query to look up.
    ///
    /// Returns `None` if the lookup failed (i.e. the entity does not exist in this query).
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(mut query: Query<(&mut Transform, &Color)>) {
    ///     if let Some(mut components) = query.get_entity(entity_id) {
    ///         let (transform, color) = components.unpack();
    ///     }
    /// }
    /// ```
    pub fn get_entity_mut(&mut self, entity_id: EntityId) -> Option<Q::Item<'a>> {
        let mut component_ptrs = MaybeUninit::<Q::DataPtrs>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe {
            _GET_ENTITY_FN.unwrap_unchecked()(
                self.handle,
                entity_id,
                (&mut component_ptrs as *mut MaybeUninit<Q::DataPtrs>).cast(),
            )
        };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_uninit_and_read_external(&mut component_ptrs, |ptr| {
                _GET_ENTITY_FN.unwrap_unchecked()(self.handle, entity_id, ptr.cast())
            })
        };

        if res == 0 {
            Some(unsafe { Q::new_item(component_ptrs.assume_init()) })
        } else {
            None
        }
    }

    /// Returns an immutable reference to a set of components in this query.
    ///
    /// `label` is the label of an entity in this query to look up.
    ///
    /// Returns `None` if the lookup failed (i.e. the label does not exist for
    /// any entities in the query).
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(query: Query<(&mut Transform, &Color)>) {
    ///     if let Some(components) = query.get_label(c"main_door") {
    ///         let (transform, color) = components.unpack();
    ///     }
    /// }
    /// ```
    pub fn get_label(&self, label: &str) -> Option<Q::ReadOnly<'a>> {
        let mut component_ptrs = MaybeUninit::<Q::DataPtrs>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe {
            _GET_LABEL_FN.unwrap_unchecked()(
                self.handle,
                &FfiStr::new(label),
                (&mut component_ptrs as *mut MaybeUninit<Q::DataPtrs>).cast(),
            )
        };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_and_write_external_slice(label.as_bytes(), |label_ptr| {
                let ffi_str = FfiStr::from_raw_parts(label_ptr, label.len());

                crate::wasm::alloc_and_write_external(&ffi_str, |ffi_str_ptr| {
                    crate::wasm::alloc_uninit_and_read_external(&mut component_ptrs, |ptr| {
                        _GET_LABEL_FN.unwrap_unchecked()(self.handle, ffi_str_ptr, ptr.cast())
                    })
                })
            })
        };

        if res == 0 {
            Some(unsafe { Q::new_read_only(component_ptrs.assume_init()) })
        } else {
            None
        }
    }

    /// Returns an mutable reference to a set of components in this query.
    ///
    /// `label` is the label of an entity in this query to look up.
    ///
    /// Returns `None` if the lookup failed (i.e. the label does not exist for
    /// any entities in the query).
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(query: Query<(&mut Transform, &Color)>) {
    ///     if let Some(components) = query.get_label(c"main_door") {
    ///         let (transform, color) = components.unpack();
    ///     }
    /// }
    /// ```
    pub fn get_label_mut(&mut self, label: &str) -> Option<Q::Item<'a>> {
        let mut component_ptrs = MaybeUninit::<Q::DataPtrs>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe {
            _GET_LABEL_FN.unwrap_unchecked()(
                self.handle,
                &FfiStr::new(label),
                (&mut component_ptrs as *mut MaybeUninit<Q::DataPtrs>).cast(),
            )
        };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            crate::wasm::alloc_and_write_external_slice(label.as_bytes(), |label_ptr| {
                let ffi_str = FfiStr::from_raw_parts(label_ptr, label.len());

                crate::wasm::alloc_and_write_external(&ffi_str, |ffi_str_ptr| {
                    crate::wasm::alloc_uninit_and_read_external(&mut component_ptrs, |ptr| {
                        _GET_LABEL_FN.unwrap_unchecked()(self.handle, ffi_str_ptr, ptr.cast())
                    })
                })
            })
        };

        if res == 0 {
            Some(unsafe { Q::new_item(component_ptrs.assume_init()) })
        } else {
            None
        }
    }

    /// Iterates over all entities in this query by calling the provided function once per entity.
    ///
    /// This function only runs on a single thread. Prefer `par_for_each` where possible
    /// (see `par_for_each` docs for details).
    ///
    /// The parameters of the function will match the order and mutability of the query template.
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(mut query: Query<(&mut Transform, &Color)>) {
    ///     query.for_each(|(transform, color)| {
    ///
    ///     });
    /// }
    /// ```
    #[cfg_attr(feature = "dynamic_wasm", allow(unused_mut))]
    pub fn for_each<F>(&mut self, mut f: F)
    where
        F: FnMut(Q::Item<'_>),
    {
        #[cfg(not(feature = "dynamic_wasm"))]
        {
            unsafe extern "C" fn callback<Q: QueryData, F: FnMut(Q::Item<'_>)>(
                entity_data: *const *const c_void,
                user_data: *mut c_void,
            ) -> c_int {
                let callback = || unsafe {
                    let component_ptrs = entity_data.cast::<Q::DataPtrs>();
                    let f = user_data.cast::<F>().as_mut().unwrap_unchecked();
                    f(Q::new_item(component_ptrs.read()));
                };

                match std::panic::catch_unwind(callback) {
                    Ok(..) => ForEachResult::Continue as i32,
                    Err(..) => ForEachResult::Error as i32,
                }
            }

            unsafe {
                let f_ptr: *mut F = &mut f as *mut _;
                _FOR_EACH_FN.unwrap_unchecked()(self.handle, callback::<Q, F>, f_ptr.cast());
            }
        }

        #[cfg(feature = "dynamic_wasm")]
        {
            self.iter_mut().for_each(f);
        }
    }

    /// Iterates over all entities in this query by calling the provided function once per entity.
    ///
    /// This version of for-each will be run in parallel and can provide significant performance improvements.
    /// This version should be the default, unless it is necessary to mutate captured state.
    ///
    /// The parameters of the function will match the order and mutability of the query template.
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(mut query: Query<(&mut Transform, &Color)>) {
    ///     query.par_for_each(|(transform, color)| {
    ///
    ///     });
    /// }
    /// ```
    pub fn par_for_each<F>(&mut self, f: F)
    where
        F: Fn(Q::Item<'_>) + Send + Sync,
    {
        #[cfg(not(feature = "dynamic_wasm"))]
        {
            unsafe extern "C" fn callback<Q: QueryData, F: Fn(Q::Item<'_>)>(
                entity_data: *const *const c_void,
                user_data: *const c_void,
            ) -> c_int {
                let callback = || unsafe {
                    let component_ptrs = entity_data.cast::<Q::DataPtrs>();
                    let f = user_data.cast::<F>().as_ref().unwrap_unchecked();
                    f(Q::new_item(component_ptrs.read()));
                };

                match std::panic::catch_unwind(callback) {
                    Ok(..) => ForEachResult::Continue as i32,
                    Err(..) => ForEachResult::Error as i32,
                }
            }

            unsafe {
                let f: *const F = &f as *const _;
                _PAR_FOR_EACH_FN.unwrap_unchecked()(self.handle, callback::<Q, F>, f.cast());
            }
        }

        #[cfg(feature = "dynamic_wasm")]
        {
            self.iter_mut().for_each(f);
        }
    }

    /// Creates an immutable [`Iterator`] over a query
    ///
    /// # Examples
    ///
    /// ```
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(query: Query<(&Transform, &Color)>) {
    ///     let object_location_descriptions = query.iter().map(|scene_object_ref| {
    ///         let (transform, _) = scene_object_ref.unpack();
    ///         format!("Object is at {:?}", transform)
    ///     }).collect::<Vec<_>>();
    /// }
    /// ```
    pub fn iter(&self) -> QueryIter<'a, '_, Q> {
        QueryIter::new(self, self.len())
    }

    /// Creates an mutable [`Iterator`] over a query
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ops::ControlFlow;
    /// use engine::{Transform, Query, colors::Color};
    ///
    /// fn my_system(mut query: Query<(&mut Transform, &Color)>) {
    ///     let expected_color = Color::new(0., 0., 1., 1.);
    ///     query.iter_mut().try_for_each(|mut scene_object_ref| {
    ///         let (transform, color) = scene_object_ref.unpack();
    ///         if **color == expected_color {
    ///             transform.position.x += 5.5;
    ///             ControlFlow::Break(())
    ///         } else {
    ///             ControlFlow::Continue(())
    ///         }
    ///     });
    /// }
    /// ```
    pub fn iter_mut(&mut self) -> QueryIterMut<'a, '_, Q> {
        QueryIterMut::new(self, self.len())
    }
}

pub struct QueryIter<'a, 'b, Q: QueryData> {
    query: &'b Query<'a, Q>,
    index: usize,
    reverse_index: usize,
}

impl<'a, 'b, Q: QueryData> QueryIter<'a, 'b, Q> {
    fn new(query: &'b Query<'a, Q>, len: usize) -> Self {
        Self {
            query,
            index: 0,
            reverse_index: len,
        }
    }
}

impl<'a, Q: QueryData> Iterator for QueryIter<'a, '_, Q> {
    type Item = Q::ReadOnly<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.reverse_index {
            return None;
        }

        let res = self.query.get(self.index)?;
        self.index += 1;
        Some(res)
    }
}

impl<'a, Q: QueryData> DoubleEndedIterator for QueryIter<'a, '_, Q> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.index > self.reverse_index {
            return None;
        }
        self.reverse_index -= 1;
        self.query.get(self.reverse_index)
    }
}

impl<'a, Q: QueryData> ExactSizeIterator for QueryIter<'a, '_, Q> {
    fn len(&self) -> usize {
        self.reverse_index - self.index
    }
}

pub struct QueryIterMut<'a, 'b, Q: QueryData> {
    query: &'b mut Query<'a, Q>,
    index: usize,
    reverse_index: usize,
}

impl<'a, 'b, Q: QueryData> QueryIterMut<'a, 'b, Q> {
    fn new(query: &'b mut Query<'a, Q>, len: usize) -> Self {
        Self {
            query,
            index: 0,
            reverse_index: len,
        }
    }
}

impl<'a, Q: QueryData> Iterator for QueryIterMut<'a, '_, Q> {
    type Item = Q::Item<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.reverse_index {
            return None;
        }

        let res = self.query.get_mut(self.index)?;
        self.index += 1;
        Some(res)
    }
}

impl<'a, Q: QueryData> DoubleEndedIterator for QueryIterMut<'a, '_, Q> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.index > self.reverse_index {
            return None;
        }
        self.reverse_index -= 1;
        self.query.get_mut(self.reverse_index)
    }
}

impl<'a, Q: QueryData> ExactSizeIterator for QueryIterMut<'a, '_, Q> {
    fn len(&self) -> usize {
        self.reverse_index - self.index
    }
}

impl<'a, 'b, Q: QueryData> IntoIterator for &'b Query<'a, Q> {
    type Item = Q::ReadOnly<'a>;

    type IntoIter = QueryIter<'a, 'b, Q>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, 'b, Q: QueryData> IntoIterator for &'b mut Query<'a, Q> {
    type Item = Q::Item<'a>;

    type IntoIter = QueryIterMut<'a, 'b, Q>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[repr(i32)]
#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ForEachResult {
    Continue = 0,
    Break = 1,
    Error = 2,
}

impl TryFrom<i32> for ForEachResult {
    type Error = Box<dyn Error + Send + Sync>;

    fn try_from(value: i32) -> Result<Self, <Self as TryFrom<i32>>::Error> {
        match value {
            0 => Ok(Self::Continue),
            1 => Ok(Self::Break),
            2 => Ok(Self::Error),
            _ => Err(format!("invalid ForEachResult error code '{value}'").into()),
        }
    }
}

pub static mut _LEN_FN: Option<unsafe extern "C" fn(*const c_void) -> usize> = None;

pub static mut _GET_FN: Option<
    unsafe extern "C" fn(*const c_void, usize, *mut *const c_void) -> i32,
> = None;

pub static mut _GET_ENTITY_FN: Option<
    unsafe extern "C" fn(*const c_void, EntityId, *mut *const c_void) -> i32,
> = None;

pub static mut _GET_LABEL_FN: Option<
    unsafe extern "C" fn(*const c_void, *const FfiStr<'_>, *mut *const c_void) -> i32,
> = None;

pub static mut _FOR_EACH_FN: Option<
    unsafe extern "C" fn(
        *mut c_void,
        unsafe extern "C" fn(*const *const c_void, *mut c_void) -> c_int,
        *mut c_void,
    ),
> = None;

pub static mut _PAR_FOR_EACH_FN: Option<
    unsafe extern "C" fn(
        *mut c_void,
        unsafe extern "C" fn(*const *const c_void, *const c_void) -> c_int,
        *const c_void,
    ),
> = None;

/// # Safety
///
/// This should not be implemented by user code.
pub unsafe trait QueryData: Sized {
    type DataPtrs;
    type Item<'a>;
    type ReadOnly<'a>;

    /// # Safety
    ///
    /// `component_ptrs` must point to a valid array of Components.
    unsafe fn new_item<'a>(component_ptrs: Self::DataPtrs) -> Self::Item<'a>;

    /// # Safety
    ///
    /// `component_ptrs` must point to a valid array of Components.
    unsafe fn new_read_only<'a>(component_ptrs: Self::DataPtrs) -> Self::ReadOnly<'a>;

    /// # Safety
    ///
    /// `component_ptr` must point to a valid Component.
    unsafe fn new_item_single<'a>(_component_ptr: *mut c_void) -> Self::Item<'a> {
        unreachable!()
    }

    /// # Safety
    ///
    /// `component_ptr` must point to a valid Component.
    unsafe fn new_read_only_single<'a>(_component_ptr: *mut c_void) -> Self::ReadOnly<'a> {
        unreachable!()
    }
}

unsafe impl<A: Component> QueryData for Ref<'_, A> {
    type DataPtrs = [*mut c_void; 1];
    type Item<'b> = Ref<'b, A>;
    type ReadOnly<'b> = Ref<'b, A>;

    unsafe fn new_item<'a>(component_ptrs: Self::DataPtrs) -> Self::Item<'a> {
        unsafe { Self::new_item_single(component_ptrs[0]) }
    }

    unsafe fn new_read_only<'a>(component_ptrs: Self::DataPtrs) -> Self::ReadOnly<'a> {
        unsafe { Self::new_read_only_single(component_ptrs[0]) }
    }

    unsafe fn new_item_single<'a>(component_ptr: *mut c_void) -> Self::Item<'a> {
        unsafe { Ref::new(NonNull::new_unchecked(component_ptr.cast())) }
    }

    unsafe fn new_read_only_single<'a>(component_ptr: *mut c_void) -> Self::ReadOnly<'a> {
        unsafe { Ref::new(NonNull::new_unchecked(component_ptr.cast())) }
    }
}

unsafe impl<A: Component> QueryData for Mut<'_, A> {
    type DataPtrs = [*mut c_void; 1];
    type Item<'b> = Mut<'b, A>;
    type ReadOnly<'b> = Ref<'b, A>;

    unsafe fn new_item<'a>(component_ptrs: Self::DataPtrs) -> Self::Item<'a> {
        unsafe { Self::new_item_single(component_ptrs[0]) }
    }

    unsafe fn new_read_only<'a>(component_ptrs: Self::DataPtrs) -> Self::ReadOnly<'a> {
        unsafe { Self::new_read_only_single(component_ptrs[0]) }
    }

    unsafe fn new_item_single<'a>(component_ptr: *mut c_void) -> Self::Item<'a> {
        unsafe { Mut::new(NonNull::new_unchecked(component_ptr.cast())) }
    }

    unsafe fn new_read_only_single<'a>(component_ptr: *mut c_void) -> Self::ReadOnly<'a> {
        unsafe { Ref::new(NonNull::new_unchecked(component_ptr.cast())) }
    }
}

macro_rules! impl_query_data_tuples {
    ($(#[$meta:meta])* $N:expr, $(($n:tt, $T:ident)),*) => {
        $(#[$meta])*
        unsafe impl<$($T: QueryData,)*> QueryData for ($($T,)*) {
            type DataPtrs = [*mut c_void; $N];
            type Item<'b> = ($($T::Item<'b>,)*);
            type ReadOnly<'b> = ($($T::ReadOnly<'b>,)*);

            unsafe fn new_item<'a>(component_ptrs: Self::DataPtrs) -> Self::Item<'a> {
                unsafe { ($($T::new_item_single(component_ptrs[$n]),)*) }
            }

            unsafe fn new_read_only<'a>(component_ptrs: Self::DataPtrs) -> Self::ReadOnly<'a> {
                unsafe { ($($T::new_read_only_single(component_ptrs[$n]),)*) }
            }
        }
    };
}

all_tuples_enumerated_with_size!(impl_query_data_tuples, 1, 15, T);
