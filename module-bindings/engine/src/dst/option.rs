use snapshot::{Deserialize, Deserializer, ReadUninit, Serialize, Serializer, WriteUninit};

#[derive(Clone, Copy, Debug, Eq, PartialOrd, Ord, serde::Deserialize)]
#[repr(C)]
pub enum EcsOption<T> {
    None,
    Some(T),
}

impl<T> From<T> for EcsOption<T> {
    fn from(value: T) -> Self {
        Self::Some(value)
    }
}

impl<T> From<Option<T>> for EcsOption<T> {
    fn from(value: Option<T>) -> Self {
        value.map_or(Self::None, Self::Some)
    }
}

impl<T> From<EcsOption<T>> for Option<T> {
    fn from(value: EcsOption<T>) -> Self {
        value.map_or(None, Some)
    }
}

impl<T> EcsOption<T> {
    #[doc(alias = "flatmap")]
    #[inline]
    pub fn and_then<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> Option<U>,
    {
        match self {
            Self::Some(x) => f(x),
            Self::None => None,
        }
    }

    #[inline]
    pub const fn as_ref(&self) -> Option<&T> {
        match *self {
            Self::Some(ref x) => Some(x),
            Self::None => None,
        }
    }

    #[inline]
    pub const fn as_mut(&mut self) -> Option<&mut T> {
        match *self {
            Self::Some(ref mut x) => Some(x),
            Self::None => None,
        }
    }

    #[must_use = "if you intended to assert that this has a value, consider `.unwrap()` instead"]
    #[inline]
    pub const fn is_some(&self) -> bool {
        matches!(*self, Self::Some(_))
    }

    #[must_use]
    #[inline]
    pub fn is_some_and(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Self::None => false,
            Self::Some(x) => f(x),
        }
    }

    #[must_use = "if you intended to assert that this doesn't have a value, consider \
                  wrapping this in an `assert!()` instead"]
    #[inline]
    pub const fn is_none(&self) -> bool {
        !self.is_some()
    }

    #[must_use]
    #[inline]
    pub fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Self::None => true,
            Self::Some(x) => f(x),
        }
    }

    #[inline]
    pub fn map<U, F>(self, f: F) -> EcsOption<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Some(x) => EcsOption::Some(f(x)),
            Self::None => EcsOption::None,
        }
    }

    #[inline]
    #[must_use = "if you don't need the returned value, use `if let` instead"]
    pub fn map_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Some(t) => f(t),
            Self::None => default,
        }
    }

    #[inline]
    pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce() -> U,
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Some(t) => f(t),
            Self::None => default(),
        }
    }

    #[inline]
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Self::Some(x) => x,
            Self::None => default,
        }
    }

    #[inline]
    #[track_caller]
    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        match self {
            Self::Some(x) => x,
            Self::None => f(),
        }
    }
}

impl<T: PartialEq> PartialEq for EcsOption<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Spelling out the cases explicitly optimizes better than `_ => false`.
        match (self, other) {
            (EcsOption::Some(l), EcsOption::Some(r)) => *l == *r,
            (EcsOption::Some(_), EcsOption::None) => false,
            (EcsOption::None, EcsOption::Some(_)) => false,
            (EcsOption::None, EcsOption::None) => true,
        }
    }
}

impl<T: Serialize> Serialize for EcsOption<T> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> snapshot::Result<()>
    where
        W: WriteUninit,
    {
        if let Self::Some(v) = self {
            true.serialize(serializer)?;
            v.serialize(serializer)
        } else {
            false.serialize(serializer)
        }
    }
}

impl<T: Deserialize> Deserialize for EcsOption<T> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> snapshot::Result<Self>
    where
        R: ReadUninit,
    {
        if unsafe { bool::deserialize(deserializer)? } {
            Ok(Self::Some(unsafe { T::deserialize(deserializer)? }))
        } else {
            Ok(Self::None)
        }
    }
}
