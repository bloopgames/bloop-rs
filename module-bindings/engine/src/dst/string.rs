use std::{
    borrow::Borrow,
    fmt::{self, Debug},
    ops::{Deref, DerefMut},
};

use snapshot_derive::{DeserializeEngine, SerializeEngine};

use crate::dst::vec::{EcsSliceMut, EcsSliceRef, EcsVec};

#[derive(SerializeEngine, DeserializeEngine)]
#[repr(transparent)]
pub struct EcsString {
    vec: EcsVec<u8>,
}

impl EcsString {
    pub fn as_str<'a>(&'a self) -> EcsStringRef<'a> {
        EcsStringRef {
            slice: self.vec.as_slice(),
        }
    }

    pub fn as_mut_str<'a>(&'a self) -> EcsStringRef<'a> {
        EcsStringRef {
            slice: self.vec.as_slice(),
        }
    }
}

impl fmt::Display for EcsString {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.as_str(), f)
    }
}

impl From<&str> for EcsString {
    fn from(value: &str) -> Self {
        Self {
            vec: value.as_bytes().iter().copied().collect(),
        }
    }
}

impl From<String> for EcsString {
    fn from(value: String) -> Self {
        Self {
            vec: value.as_bytes().iter().copied().collect(),
        }
    }
}

pub struct EcsStringRef<'a> {
    slice: EcsSliceRef<'a, u8>,
}

pub struct EcsStringMut<'a> {
    slice: EcsSliceMut<'a, u8>,
}

impl AsRef<str> for EcsStringRef<'_> {
    #[inline]
    fn as_ref(&self) -> &str {
        self
    }
}

impl AsRef<str> for EcsStringMut<'_> {
    #[inline]
    fn as_ref(&self) -> &str {
        self
    }
}

impl Borrow<str> for EcsStringRef<'_> {
    #[inline]
    fn borrow(&self) -> &str {
        self
    }
}

impl Borrow<str> for EcsStringMut<'_> {
    #[inline]
    fn borrow(&self) -> &str {
        self
    }
}

impl Deref for EcsStringRef<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { str::from_utf8_unchecked(&self.slice) }
    }
}

impl Deref for EcsStringMut<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { str::from_utf8_unchecked(&self.slice) }
    }
}

impl DerefMut for EcsStringMut<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { str::from_utf8_unchecked_mut(&mut self.slice) }
    }
}

impl PartialEq<&str> for EcsStringRef<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        PartialEq::eq(&self[..], &other[..])
    }
}

impl PartialEq<EcsStringRef<'_>> for &str {
    #[inline]
    fn eq(&self, other: &EcsStringRef<'_>) -> bool {
        PartialEq::eq(&self[..], &other[..])
    }
}

impl PartialEq<&str> for EcsString {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        PartialEq::eq(&self.as_str()[..], &other[..])
    }
}

impl PartialEq<EcsString> for &str {
    #[inline]
    fn eq(&self, other: &EcsString) -> bool {
        PartialEq::eq(&self[..], &other.as_str()[..])
    }
}

impl Debug for EcsString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}
