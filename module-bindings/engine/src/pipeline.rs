//! This library is primarily for the higher level use case of needing to manage
//! (render pipelines)[<https://en.wikipedia.org/wiki/Graphics_pipeline>]. Most
//! users will not need to worry about this, as established platforms will do
//! this work automatically as part of the engine material system.

use std::{
    ffi::c_char,
    fmt::Display,
    num::NonZero,
    ops::{Deref, DerefMut},
};

use crate::material::MaterialId;

/// A handle identifying a pipeline.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, serde::Deserialize)]
pub struct PipelineId(pub NonZero<u32>);

impl Default for PipelineId {
    fn default() -> Self {
        Self(unsafe { NonZero::new_unchecked(1) })
    }
}

impl AsRef<PipelineId> for PipelineId {
    fn as_ref(&self) -> &PipelineId {
        self
    }
}

impl Deref for PipelineId {
    type Target = NonZero<u32>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PipelineId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for PipelineId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<NonZero<u32>> for PipelineId {
    fn from(value: NonZero<u32>) -> Self {
        Self(value)
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct PendingPipeline {
    pub id: PipelineId,
    pub material_id: MaterialId,
}

#[repr(C)]
#[derive(Debug)]
pub struct EnginePipeline {
    pub id: PipelineId,
    pub material_id: MaterialId,
}

#[repr(C)]
#[derive(Debug)]
pub struct LoadedPipeline {
    pub id: PipelineId,
    pub material_id: MaterialId,
}

#[repr(C)]
#[derive(Debug)]
pub struct FailedPipeline {
    pub failure_reason: *const c_char,
    pub id: PipelineId,
    pub material_id: MaterialId,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PipelineType {
    Pending,
    Engine,
    Loaded,
    Failed,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetPipelineTypeByIdStatus {
    Success,
    PipelineAssetManagerNull,
    PipelineTypeNull,
    PipelineIdNotFound,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetPipelineByIdStatus {
    Success,
    PipelineAssetManagerNull,
    OutputPipelineNull,
    PipelineIdNotFound,
    PipelineTypeIncorrect,
}

#[repr(u32)]
#[derive(Debug)]
pub enum LoadPipelineStatus {
    Success,
    PipelineAssetManagerNull,
    OutputPendingPipelineNull,
}

#[repr(u32)]
#[derive(Debug)]
pub enum LoadPipelineByPendingPipelineStatus {
    Success,
    PipelineAssetManagerNull,
}
