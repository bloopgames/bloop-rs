use std::{
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr, slice,
};

use flatbuffers::{Follow, Push};

use crate::{ComponentId, EcsType};

pub type TaskId = u32;

pub trait Callable: EcsType {
    type Parameters<'a>: Follow<'a>;
    type ReturnValue<'a>: Follow<'a>;
}

pub trait AsyncCompletion: EcsType {
    type Function: Callable;
    type UserData<'a>: Follow<'a>;
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Pod<T: Copy + 'static>(pub T);

impl<'a, T: Copy + 'static> Follow<'a> for Pod<T> {
    type Inner = T;

    unsafe fn follow(buf: &'a [u8], loc: usize) -> Self::Inner {
        let data = &buf[loc..loc + size_of::<T>()];

        let mut mem = MaybeUninit::<T>::uninit();
        // Since [u8] has alignment 1, we copy it into T which may have higher alignment.
        unsafe {
            ptr::copy_nonoverlapping(data.as_ptr(), mem.as_mut_ptr().cast(), size_of::<T>());
            mem.assume_init()
        }
    }
}

impl<T: Copy + 'static> Push for Pod<T> {
    type Output = Pod<T>;

    unsafe fn push(&self, dst: &mut [u8], _written_len: usize) {
        unsafe {
            ptr::copy_nonoverlapping(
                (self as *const Self).cast::<u8>(),
                dst.as_mut_ptr().cast(),
                size_of::<T>(),
            );
        };
    }
}

impl<T: Copy + 'static> Deref for Pod<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Copy + 'static> DerefMut for Pod<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Copy + 'static> From<T> for Pod<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

pub struct Completion<F> {
    marker: PhantomData<F>,
}

impl<F> Completion<F> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<F> Default for Completion<F> {
    fn default() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

impl<F: AsyncCompletion> Completion<F> {
    pub fn len_fb(&self) -> usize {
        unsafe { _COMPLETION_COUNT_FN.unwrap_unchecked()(F::id()) }
    }

    pub fn is_empty_fb(&self) -> bool {
        self.len_fb() == 0
    }

    #[allow(clippy::type_complexity)]
    pub fn get(
        &self,
        index: usize,
    ) -> Option<(
        <<F::Function as Callable>::ReturnValue<'_> as Follow<'_>>::Inner,
        <F::UserData<'_> as Follow<'_>>::Inner,
    )> {
        unsafe {
            let completion = _COMPLETION_GET_FN.unwrap_unchecked()(F::id(), index);

            if completion.return_value_ptr.is_null() || completion.user_data_ptr.is_null() {
                return None;
            }

            let return_value = slice::from_raw_parts(
                completion.return_value_ptr.cast(),
                completion.return_value_size,
            );
            let return_value = flatbuffers::root_unchecked::<
                <F::Function as Callable>::ReturnValue<'_>,
            >(return_value);

            let user_data =
                slice::from_raw_parts(completion.user_data_ptr.cast(), completion.user_data_size);
            let user_data = flatbuffers::root_unchecked::<F::UserData<'_>>(user_data);

            Some((return_value, user_data))
        }
    }
}

impl<'a, F: AsyncCompletion> IntoIterator for &'a Completion<F> {
    type Item = (
        <<F::Function as Callable>::ReturnValue<'a> as Follow<'a>>::Inner,
        <F::UserData<'a> as Follow<'a>>::Inner,
    );

    type IntoIter = CompletionIter<'a, F>;

    fn into_iter(self) -> Self::IntoIter {
        CompletionIter {
            i: 0,
            completion: self,
        }
    }
}

pub struct CompletionIter<'a, F: AsyncCompletion> {
    i: usize,
    completion: &'a Completion<F>,
}

impl<'a, F: AsyncCompletion> Iterator for CompletionIter<'a, F> {
    type Item = (
        <<F::Function as Callable>::ReturnValue<'a> as Follow<'a>>::Inner,
        <F::UserData<'a> as Follow<'a>>::Inner,
    );

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.i;
        self.i += 1;
        self.completion.get(index)
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct AsyncCompletionValue {
    pub return_value_ptr: *const MaybeUninit<u8>,
    pub return_value_size: usize,
    pub user_data_ptr: *const MaybeUninit<u8>,
    pub user_data_size: usize,
}

impl AsyncCompletionValue {
    pub fn null() -> Self {
        Self {
            return_value_ptr: ptr::null(),
            return_value_size: 0,
            user_data_ptr: ptr::null(),
            user_data_size: 0,
        }
    }

    pub fn is_null(&self) -> bool {
        self.return_value_ptr.is_null()
            || self.return_value_size == 0
            || self.user_data_ptr.is_null()
            || self.user_data_size == 0
    }
}

pub static mut _COMPLETION_COUNT_FN: Option<unsafe extern "C" fn(ComponentId) -> usize> = None;

pub static mut _COMPLETION_GET_FN: Option<
    unsafe extern "C" fn(ComponentId, usize) -> AsyncCompletionValue,
> = None;
