use std::{
    error::Error,
    ffi::c_char,
    fmt::Display,
    num::NonZero,
    ops::{Deref, DerefMut},
};

use snapshot_derive::{DeserializeEngine, SerializeEngine};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Deserialize)]
pub enum TextAlignment {
    #[default]
    Left,
    Center,
    Right,
}

impl From<crate::event::graphics::TextAlignment> for TextAlignment {
    fn from(value: crate::event::graphics::TextAlignment) -> Self {
        match value {
            crate::event::graphics::TextAlignment::Left => TextAlignment::Left,
            crate::event::graphics::TextAlignment::Center => TextAlignment::Center,
            crate::event::graphics::TextAlignment::Right => TextAlignment::Right,
            _ => unreachable!(),
        }
    }
}

#[repr(transparent)]
#[derive(
    Clone,
    Copy,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    serde::Deserialize,
    serde::Serialize,
    DeserializeEngine,
    SerializeEngine,
)]
pub struct TextId(pub NonZero<u32>);

impl TryFrom<u32> for TextId {
    type Error = Box<dyn Error + Send + Sync>;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        NonZero::new(value)
            .ok_or("Value for TextId is 0, which is not allowed".into())
            .map(TextId)
    }
}

impl Deref for TextId {
    type Target = NonZero<u32>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TextId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<NonZero<u32>> for TextId {
    fn from(value: NonZero<u32>) -> Self {
        Self(value)
    }
}

impl Display for TextId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TextType {
    Pending,
    Engine,
    Loaded,
    Failed,
}

#[repr(C)]
#[derive(Debug)]
pub struct PendingText {
    pub text_path: *const c_char,
    pub id: TextId,
    pub set_up_watcher: bool,
}

#[repr(C)]
#[derive(Debug)]
pub struct EngineText {
    pub text_path: *const c_char,
    pub format: *const c_char,
    pub raw_text: *const c_char,
    pub id: TextId,
}

#[repr(C)]
#[derive(Debug)]
pub struct LoadedText {
    pub text_path: *const c_char,
    pub format: *const c_char,
    pub raw_text: *const c_char,
    pub id: TextId,
    pub watcher_set_up: bool,
}

#[repr(C)]
#[derive(Debug)]
pub struct FailedText {
    pub failure_reason: *const c_char,
    pub text_path: *const c_char,
    pub id: TextId,
}

#[repr(u32)]
#[derive(Debug)]
pub enum CreatePendingTextStatus {
    Success,
    OutputPendingTextNull,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetTextTypeByIdStatus {
    Success,
    TextAssetManagerNull,
    TextTypeNull,
    TextIdNotFound,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetTextByIdStatus {
    Success,
    TextAssetManagerNull,
    OutputTextNull,
    TextIdNotFound,
    TextTypeIncorrect,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetTextTypeByPathStatus {
    Success,
    TextPathNull,
    TextAssetManagerNull,
    TextTypeNull,
    TextPathNotFound,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetTextByPathStatus {
    Success,
    TextPathNull,
    TextAssetManagerNull,
    OutputTextNull,
    TextIdNotFound,
    TextTypeIncorrect,
}

#[repr(u32)]
#[derive(Debug)]
pub enum LoadTextStatus {
    Success,
    TextAssetManagerNull,
    OutputPendingTextNull,
    LoadTextError,
}

#[repr(u32)]
#[derive(Debug)]
pub enum LoadTextByPendingTextStatus {
    Success,
    PendingTextNull,
    TextAssetManagerNull,
    LoadTextError,
}
