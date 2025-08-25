//! This crate is largely inspired by Serde. We could not use Serde directly
//! because of incompatibilities with `MaybeUninit` types used throughout the
//! engine.
//!
//! This serialization implementation is similar to Serde, but simplifies
//! certain aspects of the Serde API which are unnecessary for our use-case. It
//! adds support for working with `MaybeUninit` data, as well as an additional
//! `SerializeMut` trait intended to optimize serializing interior-mutable
//! atomic types commonly used throughout the engine. It also does not use a
//! visitor pattern, as we don't need to support deserializing a type from
//! multiple possible representations.

use std::{
    array::from_fn,
    cmp,
    collections::HashMap,
    ffi::{CStr, CString, c_void},
    fmt::{Display, Formatter},
    hash::Hash,
    marker::PhantomData,
    mem::{MaybeUninit, transmute},
    num::{NonZero, Wrapping},
    slice,
    sync::Arc,
};

use aligned_vec::AVec;
use atomic_refcell::AtomicRefCell;
use glam::{Mat2, Mat3, Mat4, Vec2, Vec3, Vec4};
pub use snapshot_derive::{Deserialize, Serialize, SerializeMut};

#[cfg(test)]
mod tests;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Deserialize(Box<dyn std::error::Error + Send + Sync>),
    Read(Box<dyn std::error::Error + Send + Sync>),
    Serialize(Box<dyn std::error::Error + Send + Sync>),
    Write(Box<dyn std::error::Error + Send + Sync>),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let (error_kind, error) = match self {
            Error::Deserialize(error) => ("Deserialize", error),
            Error::Read(error) => ("Read", error),
            Error::Serialize(error) => ("Serialize", error),
            Error::Write(error) => ("Write", error),
        };

        f.write_fmt(format_args!(
            "snapshot error: Kind = {error_kind}, Error = {error}"
        ))
    }
}

impl std::error::Error for Error {}

pub struct Deserializer<R: ReadUninit> {
    reader: R,
}

impl<R: ReadUninit> Deserializer<R> {
    pub fn new(reader: R) -> Self {
        Self { reader }
    }

    /// Deserializes a `Copy + 'static` (i.e. "plain-old-data") type.
    ///
    /// # Safety
    ///
    /// `deserializer` must be at the correct read position. There are no
    /// runtime checks ensuring that the read position is correct.
    pub unsafe fn deserialize_pod<T: Copy + 'static>(&mut self) -> Result<T> {
        let mut val = MaybeUninit::uninit();

        let buf = unsafe {
            slice::from_raw_parts_mut((&mut val as *mut MaybeUninit<T>).cast(), size_of::<T>())
        };

        self.reader.read_exact(buf)?;
        Ok(unsafe { val.assume_init() })
    }

    /// Deserialize a `Vec` containing `Copy + 'static` (i.e. "plain-old-data")
    /// types.
    ///
    /// This is an optimization of the generic
    /// `impl<T: Deserialize> Deserialize for Vec<T>`.
    ///
    /// # Safety
    ///
    /// `deserializer` must be at the correct read position. There are no
    /// runtime checks ensuring that the read position is correct.
    pub unsafe fn deserialize_pod_vec<T: Copy + 'static>(&mut self) -> Result<Vec<T>> {
        let len = unsafe { usize::deserialize(self)? };

        let mut v = Vec::with_capacity(len);

        let spare_capacity = &mut v.spare_capacity_mut()[..len];
        let spare_capacity = cast_uninit_slice_mut(spare_capacity);
        self.deserialize_into_uninit_bytes(spare_capacity)?;

        unsafe {
            v.set_len(len);
        }

        Ok(v)
    }

    /// Deserialize a `Vec` containing `Copy + 'static` (i.e. "plain-old-data")
    /// types in-place.
    ///
    /// This is an optimization of the generic
    /// `impl<T: Deserialize> Deserialize for Vec<T>`.
    ///
    /// # Safety
    ///
    /// `deserializer` must be at the correct read position. There are no
    /// runtime checks ensuring that the read position is correct.
    pub unsafe fn deserialize_pod_vec_in_place<T: Copy + 'static>(
        &mut self,
        v: &mut Vec<T>,
    ) -> Result<()> {
        let len = unsafe { usize::deserialize(self)? };

        v.clear();
        v.reserve(len);

        let spare_capacity = &mut v.spare_capacity_mut()[..len];
        let spare_capacity = cast_uninit_slice_mut(spare_capacity);
        self.deserialize_into_uninit_bytes(spare_capacity)?;

        unsafe {
            v.set_len(len);
        }

        Ok(())
    }

    pub fn deserialize_into_uninit_bytes(&mut self, buf: &mut [MaybeUninit<u8>]) -> Result<()> {
        self.reader.read_exact(buf)
    }

    pub fn deserialize_uninit_bytes(&mut self, bytes: usize) -> Result<Vec<MaybeUninit<u8>>> {
        let mut buf = Vec::with_capacity(bytes);
        buf.resize(bytes, MaybeUninit::uninit());
        self.reader.read_exact(&mut buf)?;
        Ok(buf)
    }
}

pub struct Serializer<W: WriteUninit> {
    writer: W,
}

impl<W: WriteUninit> Serializer<W> {
    pub fn new(writer: W) -> Self {
        Self { writer }
    }

    /// Serialize a `Copy + 'static` (i.e. "plain-old-data") type. This also
    /// covers primitives.
    ///
    /// Note: this should *not* be used to serialize `usize`, as the size of
    /// `usize` is platform-dependent.
    pub fn serialize_pod<T: Copy + 'static>(&mut self, v: &T) -> Result<()> {
        let buf = unsafe { slice::from_raw_parts((v as *const T).cast(), size_of::<T>()) };
        self.writer.write_all(buf)
    }

    /// Serialize a `Vec` containing `Copy + 'static` (i.e. "plain-old-data")
    /// types.
    ///
    /// This is an optimization of the generic
    /// `impl<T: Serialize> Serialize for Vec<T>`.
    pub fn serialize_pod_vec<T: Copy + 'static>(&mut self, v: &Vec<T>) -> Result<()> {
        v.len().serialize(self)?;

        let data = v.as_slice();
        // Convert to `[MaybeUninit<T>]`.
        let data = slice_as_uninit(data);
        // Convert to `[MaybeUninit<u8>]`.
        let data = cast_uninit_slice(data);
        self.serialize_uninit_bytes(data)
    }

    /// Serializes a slice of bytes.
    ///
    /// Note: this does *not* encode the length.
    pub fn serialize_bytes(&mut self, v: &[u8]) -> Result<()> {
        let v = unsafe { transmute::<&[u8], &[MaybeUninit<u8>]>(v) };
        self.writer.write_all(v)
    }

    /// Serializes a slice of possibly-uninitialized bytes.
    ///
    /// Note: this does *not* encode the length.
    pub fn serialize_uninit_bytes(&mut self, v: &[MaybeUninit<u8>]) -> Result<()> {
        self.writer.write_all(v)
    }

    pub fn into_writer(self) -> W {
        self.writer
    }
}

/// Adapted from std's `trait Read`.
pub trait ReadUninit {
    fn read(&mut self, buf: &mut [MaybeUninit<u8>]) -> Result<usize>;

    fn read_exact(&mut self, mut buf: &mut [MaybeUninit<u8>]) -> Result<()> {
        while !buf.is_empty() {
            match self.read(buf) {
                Ok(0) => break,
                Ok(n) => {
                    buf = &mut buf[n..];
                }
                Err(e) => return Err(e),
            }
        }
        if !buf.is_empty() {
            Err(Error::Read("failed to fill whole buffer".into()))
        } else {
            Ok(())
        }
    }
}

/// Adapted from std's `impl Read for &[u8]`.
impl ReadUninit for &[MaybeUninit<u8>] {
    #[inline]
    fn read(&mut self, buf: &mut [MaybeUninit<u8>]) -> Result<usize> {
        let amt = cmp::min(buf.len(), self.len());
        let (a, b) = self.split_at(amt);

        // First check if the amount of bytes we want to read is small:
        // `copy_from_slice` will generally expand to a call to `memcpy`, and
        // for a single byte the overhead is significant.
        if amt == 1 {
            buf[0] = a[0];
        } else {
            buf[..amt].copy_from_slice(a);
        }

        *self = b;
        Ok(amt)
    }
}

/// Used for FFI deserialization. The FFI function is a C equivalent to
/// `ReadUninit::read()`.
///
/// If the FFI function returns a non-negative value, it represents the number
/// of bytes read. If the FFI function returns a negative number, it
/// represents an error code.
pub struct FfiReader {
    reader: *mut c_void,
    read: unsafe extern "C" fn(reader: *mut c_void, buf: *mut c_void, len: usize) -> isize,
}

impl FfiReader {
    pub fn new(
        reader: *mut c_void,
        read: unsafe extern "C" fn(reader: *mut c_void, buf: *mut c_void, len: usize) -> isize,
    ) -> Self {
        Self { reader, read }
    }
}

impl ReadUninit for FfiReader {
    #[inline]
    fn read(&mut self, buf: &mut [MaybeUninit<u8>]) -> Result<usize> {
        let res = unsafe { (self.read)(self.reader, buf.as_mut_ptr().cast(), buf.len()) };

        if res >= 0 {
            Ok(res as usize)
        } else {
            Err(Error::Read(format!("read over FFI error: {res}").into()))
        }
    }
}

/// Adapted from std's `trait Write`.
pub trait WriteUninit {
    fn write(&mut self, buf: &[MaybeUninit<u8>]) -> Result<usize>;

    fn flush(&mut self) -> Result<()>;

    fn write_all(&mut self, mut buf: &[MaybeUninit<u8>]) -> Result<()> {
        while !buf.is_empty() {
            match self.write(buf) {
                Ok(0) => {
                    return Err(Error::Write("failed to write whole buffer".into()));
                }
                Ok(n) => buf = &buf[n..],
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }
}

/// Adapted from std's `impl Write for Vec<u8>`.
impl WriteUninit for Vec<MaybeUninit<u8>> {
    #[inline]
    fn write(&mut self, buf: &[MaybeUninit<u8>]) -> Result<usize> {
        self.extend_from_slice(buf);
        Ok(buf.len())
    }

    #[inline]
    fn flush(&mut self) -> Result<()> {
        Ok(())
    }
}

/// Used for FFI serialization. The FFI function is a C equivalent to
/// `WriteUninit::write()`.
///
/// If the FFI function returns a non-negative value, it represents the number
/// of bytes written. If the FFI function returns a negative number, it
/// represents an error code.
pub struct FfiWriter {
    writer: *mut c_void,
    write: unsafe extern "C" fn(writer: *mut c_void, buf: *const c_void, len: usize) -> isize,
}

impl FfiWriter {
    pub fn new(
        writer: *mut c_void,
        write: unsafe extern "C" fn(writer: *mut c_void, buf: *const c_void, len: usize) -> isize,
    ) -> Self {
        Self { writer, write }
    }
}

impl WriteUninit for FfiWriter {
    #[inline]
    fn write(&mut self, buf: &[MaybeUninit<u8>]) -> Result<usize> {
        let res = unsafe { (self.write)(self.writer, buf.as_ptr().cast(), buf.len()) };

        if res >= 0 {
            Ok(res as usize)
        } else {
            Err(Error::Write(format!("write over FFI error: {res}").into()))
        }
    }

    #[inline]
    fn flush(&mut self) -> Result<()> {
        Ok(())
    }
}

pub trait Serialize {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit;
}

pub trait SerializeMut {
    /// We offer `SerializeMut` to allow free access to the many atomic types used by the engine,
    /// given that we will have exclusive access to all engine state when serializing.
    ///
    /// Implementers should still treat data as immutable.
    ///
    /// If implementers do not need mutable access to access interior-mutable
    /// types, `Serialize` should be preferred.
    fn serialize_mut<W>(&mut self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit;
}

pub trait Deserialize: Sized {
    /// # Safety
    ///
    /// `deserializer` must be at the correct read position.
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit;

    /// # Safety
    ///
    /// `deserializer` must be at the correct read position.
    unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        *self = unsafe { Self::deserialize(deserializer)? };
        Ok(())
    }
}

/// Assumes that a `Vec` containing `MaybeUninit<T>` is fully initialized.
///
/// # Safety
///
/// It is up to the caller to guarantee that the elements of the `Vec` really
/// are in an initialized state. Calling this when the elements are not yet
/// fully initialized causes immediate undefined behavior.
pub unsafe fn vec_assume_init<T>(v: Vec<MaybeUninit<T>>) -> Vec<T> {
    unsafe { transmute::<Vec<MaybeUninit<T>>, Vec<T>>(v) }
}

pub fn slice_as_uninit<T>(v: &[T]) -> &[MaybeUninit<T>] {
    unsafe { transmute::<&[T], &[MaybeUninit<T>]>(v) }
}

/// Copied from bytemuck's internal `try_cast_slice` implementation, adapted
/// for usage with `MaybeUninit` slices.
#[inline]
#[track_caller]
pub fn cast_uninit_slice<A: Copy, B: Copy>(a: &[MaybeUninit<A>]) -> &[MaybeUninit<B>] {
    unsafe {
        let input_bytes = size_of_val::<[MaybeUninit<A>]>(a);

        if align_of::<B>() > align_of::<A>() && !a.as_ptr().cast::<B>().is_aligned() {
            panic!("input alignment of A not aligned with B");
        } else if size_of::<B>() == size_of::<A>() {
            slice::from_raw_parts(a.as_ptr().cast::<MaybeUninit<B>>(), a.len())
        } else if (size_of::<B>() != 0 && input_bytes % size_of::<B>() == 0)
            || (size_of::<B>() == 0 && input_bytes == 0)
        {
            let new_len = if size_of::<B>() != 0 {
                input_bytes / size_of::<B>()
            } else {
                0
            };
            slice::from_raw_parts(a.as_ptr().cast::<MaybeUninit<B>>(), new_len)
        } else {
            panic!("input and output slices have different sizes");
        }
    }
}

/// Copied from bytemuck's internal `try_cast_slice_mut` implementation, adapted
/// for usage with `MaybeUninit` slices.
#[inline]
#[track_caller]
pub fn cast_uninit_slice_mut<A: Copy, B: Copy>(a: &mut [MaybeUninit<A>]) -> &mut [MaybeUninit<B>] {
    unsafe {
        let input_bytes = size_of_val::<[MaybeUninit<A>]>(a);

        if align_of::<B>() > align_of::<A>() && !a.as_ptr().cast::<B>().is_aligned() {
            panic!("input alignment of A not aligned with B");
        } else if size_of::<B>() == size_of::<A>() {
            slice::from_raw_parts_mut(a.as_mut_ptr().cast::<MaybeUninit<B>>(), a.len())
        } else if (size_of::<B>() != 0 && input_bytes % size_of::<B>() == 0)
            || (size_of::<B>() == 0 && input_bytes == 0)
        {
            let new_len = if size_of::<B>() != 0 {
                input_bytes / size_of::<B>()
            } else {
                0
            };
            slice::from_raw_parts_mut(a.as_mut_ptr().cast::<MaybeUninit<B>>(), new_len)
        } else {
            panic!("input and output slices have different sizes");
        }
    }
}

// ------------ type implementations

macro_rules! pod_impl {
    ($ty:path) => {
        impl Serialize for $ty {
            #[inline]
            fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
            where
                W: WriteUninit,
            {
                serializer.serialize_pod(self)
            }
        }

        impl SerializeMut for $ty {
            #[inline]
            fn serialize_mut<W>(&mut self, serializer: &mut Serializer<W>) -> Result<()>
            where
                W: WriteUninit,
            {
                serializer.serialize_pod(self)
            }
        }

        impl Deserialize for $ty {
            #[inline]
            unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
            where
                R: ReadUninit,
            {
                unsafe { deserializer.deserialize_pod() }
            }
        }
    };
}

pod_impl!(bool);
pod_impl!(u8);
pod_impl!(i8);
pod_impl!(u16);
pod_impl!(i16);
pod_impl!(u32);
pod_impl!(i32);
pod_impl!(u64);
pod_impl!(i64);
pod_impl!(Wrapping<u8>);
pod_impl!(Wrapping<i8>);
pod_impl!(Wrapping<u16>);
pod_impl!(Wrapping<i16>);
pod_impl!(Wrapping<u32>);
pod_impl!(Wrapping<i32>);
pod_impl!(Wrapping<u64>);
pod_impl!(Wrapping<i64>);
pod_impl!(NonZero<u8>);
pod_impl!(NonZero<i8>);
pod_impl!(NonZero<u16>);
pod_impl!(NonZero<i16>);
pod_impl!(NonZero<u32>);
pod_impl!(NonZero<i32>);
pod_impl!(NonZero<u64>);
pod_impl!(NonZero<i64>);
pod_impl!(f32);
pod_impl!(f64);

pod_impl!(Vec2);
pod_impl!(Vec3);
pod_impl!(Vec4);
pod_impl!(Mat2);
pod_impl!(Mat3);
pod_impl!(Mat4);

macro_rules! tuple_impls {
    ($($len:expr => ($($n:tt $name:ident)+))+) => {
        $(
            impl<$($name),+> Serialize for ($($name,)+)
            where
                $($name: Serialize,)+
            {
                tuple_impl_serialize_body!($len => ($($n)+));
            }

            impl<$($name),+> Deserialize for ($($name,)+)
            where
                $($name: Deserialize,)+
            {
                tuple_impl_deserialize_body!($len => ($($n $name)+));
            }
        )+
    };
}

macro_rules! tuple_impl_serialize_body {
    ($len:expr => ($($n:tt)+)) => {
        #[inline]
        fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
        where
            W: WriteUninit,
        {
            $(
                self.$n.serialize(serializer)?;
            )+

            Ok(())
        }
    };
}

macro_rules! tuple_impl_deserialize_body {
    ($len:expr => ($($n:tt $name:ident)+)) => {
        #[inline]
        unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
        where
            R: ReadUninit,
        {
            Ok(($(
                unsafe { $name::deserialize(deserializer)? },
            )+))
        }

        #[inline]
        unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
        where
            R: ReadUninit,
        {
            $(
                unsafe { self.$n.deserialize_in_place(deserializer)?; }
            )+

            Ok(())
        }
    };
}

tuple_impls! {
    1 => (0 T0)
    2 => (0 T0 1 T1)
    3 => (0 T0 1 T1 2 T2)
    4 => (0 T0 1 T1 2 T2 3 T3)
    5 => (0 T0 1 T1 2 T2 3 T3 4 T4)
    6 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5)
    7 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6)
    8 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7)
    9 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8)
    10 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9)
    11 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10)
    12 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11)
    13 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12)
    14 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13)
    15 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14)
    16 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14 15 T15)
}

impl Serialize for usize {
    /// We serialize `usize` as `u32` to support cross-platform sizes.
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        let val: u32 = (*self).try_into().map_err(|err| {
            let msg = format!("Serializer::serialize_usize({self}) overflowed max length\n{err}");
            Error::Serialize(msg.into())
        })?;
        serializer.serialize_pod(&val)
    }
}

impl Deserialize for usize {
    /// We serialize `usize` as `u32` to support cross-platform sizes.
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let size = unsafe { deserializer.deserialize_pod::<u32>()? };

        // Guaranteed to succeed, we don't support < 32-bit platforms
        Ok(size.try_into().unwrap())
    }
}

impl<T: Serialize, const N: usize> Serialize for [T; N] {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        for elem in self {
            elem.serialize(serializer)?;
        }

        Ok(())
    }
}

impl<T: Deserialize, const N: usize> Deserialize for [T; N] {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        Ok(from_fn(|_| unsafe {
            // We can't return an error within this closure, but we also can't
            // use `MaybeUninit` array initialization for const generics. So we
            // just panic ¯\_(ツ)_/¯.
            T::deserialize(deserializer).expect("panic while deserializing array element")
        }))
    }

    unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        for elem in self {
            unsafe {
                elem.deserialize_in_place(deserializer)?;
            }
        }

        Ok(())
    }
}

impl<T: Serialize> Serialize for Option<T> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        if let Some(v) = &self {
            true.serialize(serializer)?;
            v.serialize(serializer)
        } else {
            false.serialize(serializer)
        }
    }
}

impl<T: Deserialize> Deserialize for Option<T> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        if unsafe { bool::deserialize(deserializer)? } {
            Ok(Some(unsafe { T::deserialize(deserializer)? }))
        } else {
            Ok(None)
        }
    }
}

impl Serialize for Arc<CStr> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        let bytes = self.to_bytes();
        bytes.len().serialize(serializer)?;
        serializer.serialize_bytes(bytes)
    }
}

impl Deserialize for Arc<CStr> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        Ok(unsafe { CString::deserialize(deserializer)?.into() })
    }
}

impl Serialize for CString {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        let bytes = self.as_bytes();
        bytes.len().serialize(serializer)?;
        serializer.serialize_bytes(bytes)
    }
}

impl Deserialize for CString {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        unsafe {
            let len = usize::deserialize(deserializer)?;
            let bytes = deserializer.deserialize_uninit_bytes(len)?;
            let bytes = vec_assume_init(bytes);
            Ok(CString::from_vec_unchecked(bytes))
        }
    }
}

impl Serialize for &str {
    /// There is no `Deserialize` implementation for `&str`. Use `String`
    /// for deserialization.
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        let bytes = self.as_bytes();
        bytes.len().serialize(serializer)?;
        serializer.serialize_bytes(bytes)
    }
}

impl Serialize for Arc<str> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.as_ref().serialize(serializer)
    }
}

impl Deserialize for Arc<str> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        Ok(unsafe { String::deserialize(deserializer)?.into() })
    }
}

impl Serialize for String {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.as_str().serialize(serializer)
    }
}

impl Deserialize for String {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let bytes = unsafe { deserializer.deserialize_pod_vec()? };

        unsafe { Ok(String::from_utf8_unchecked(bytes)) }
    }

    unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        unsafe { deserializer.deserialize_pod_vec_in_place(self.as_mut_vec()) }
    }
}

impl Serialize for Box<[MaybeUninit<u8>]> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.len().serialize(serializer)?;
        serializer.serialize_uninit_bytes(self)
    }
}

impl Deserialize for Box<[MaybeUninit<u8>]> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let len = unsafe { usize::deserialize(deserializer)? };
        Ok(deserializer.deserialize_uninit_bytes(len)?.into())
    }
}

impl<T: Serialize> Serialize for Vec<T> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.len().serialize(serializer)?;

        for elem in self {
            elem.serialize(serializer)?;
        }

        Ok(())
    }
}

impl<T: SerializeMut> SerializeMut for Vec<T> {
    fn serialize_mut<W>(&mut self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.len().serialize(serializer)?;

        for elem in self {
            elem.serialize_mut(serializer)?;
        }

        Ok(())
    }
}

impl<T: Deserialize> Deserialize for Vec<T> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let len = unsafe { usize::deserialize(deserializer)? };
        let mut vec = Vec::with_capacity(len);

        for _ in 0..len {
            let elem = unsafe { T::deserialize(deserializer)? };
            vec.push(elem);
        }

        Ok(vec)
    }

    unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        let len = unsafe { usize::deserialize(deserializer)? };

        let existing_len = len.min(self.len());
        let additional_len = len.saturating_sub(self.len());

        self.reserve(additional_len);

        // Initialize existing elements in-place.
        for elem in &mut self[..existing_len] {
            unsafe { elem.deserialize_in_place(deserializer)? };
        }

        // Use deserialize() to push new elements
        for _ in 0..additional_len {
            let elem = unsafe { T::deserialize(deserializer)? };
            self.push(elem);
        }

        // Resize in case deserialized length is shorter than existing length.
        // This will always resize smaller, so we can just use `unreachable!()`.
        self.resize_with(len, || unreachable!());

        Ok(())
    }
}

impl Serialize for AVec<MaybeUninit<u8>> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.len().serialize(serializer)?;
        self.alignment().serialize(serializer)?;
        serializer.serialize_uninit_bytes(self)
    }
}

impl Deserialize for AVec<MaybeUninit<u8>> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let len = unsafe { usize::deserialize(deserializer)? };
        let align = unsafe { usize::deserialize(deserializer)? };

        let mut vec = AVec::with_capacity(align, len);

        vec.resize(len, MaybeUninit::uninit());
        deserializer.deserialize_into_uninit_bytes(&mut vec)?;

        Ok(vec)
    }

    unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        let len = unsafe { usize::deserialize(deserializer)? };
        let align = unsafe { usize::deserialize(deserializer)? };

        if self.alignment() != align {
            *self = AVec::with_capacity(align, len);
        }

        self.resize(len, MaybeUninit::uninit());
        deserializer.deserialize_into_uninit_bytes(self)?;

        Ok(())
    }
}

impl<K: Serialize, V: Serialize> Serialize for HashMap<K, V> {
    fn serialize<W>(&self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.len().serialize(serializer)?;

        for (k, v) in self {
            k.serialize(serializer)?;
            v.serialize(serializer)?;
        }

        Ok(())
    }
}

impl<K: Serialize, V: SerializeMut> SerializeMut for HashMap<K, V> {
    fn serialize_mut<W>(&mut self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.len().serialize(serializer)?;

        for (k, v) in self {
            k.serialize(serializer)?;
            v.serialize_mut(serializer)?;
        }

        Ok(())
    }
}

impl<K: Deserialize + Eq + Hash, V: Deserialize> Deserialize for HashMap<K, V> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let len = unsafe { usize::deserialize(deserializer)? };
        let mut map = HashMap::with_capacity(len);

        for _ in 0..len {
            let k = unsafe { K::deserialize(deserializer)? };
            let v = unsafe { V::deserialize(deserializer)? };
            map.insert(k, v);
        }

        Ok(map)
    }
}

impl<T: Serialize> SerializeMut for AtomicRefCell<T> {
    fn serialize_mut<W>(&mut self, serializer: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        self.get_mut().serialize(serializer)
    }
}

impl<T: Deserialize> Deserialize for AtomicRefCell<T> {
    unsafe fn deserialize<R>(deserializer: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        let val = unsafe { T::deserialize(deserializer)? };
        Ok(AtomicRefCell::new(val))
    }

    unsafe fn deserialize_in_place<R>(&mut self, deserializer: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        unsafe { self.get_mut().deserialize_in_place(deserializer) }
    }
}

impl<T: ?Sized> Serialize for PhantomData<T> {
    fn serialize<W>(&self, _: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        Ok(())
    }
}

impl<T: ?Sized> SerializeMut for PhantomData<T> {
    fn serialize_mut<W>(&mut self, _: &mut Serializer<W>) -> Result<()>
    where
        W: WriteUninit,
    {
        Ok(())
    }
}

impl<T: ?Sized> Deserialize for PhantomData<T> {
    unsafe fn deserialize<R>(_: &mut Deserializer<R>) -> Result<Self>
    where
        R: ReadUninit,
    {
        Ok(PhantomData)
    }

    unsafe fn deserialize_in_place<R>(&mut self, _: &mut Deserializer<R>) -> Result<()>
    where
        R: ReadUninit,
    {
        Ok(())
    }
}
