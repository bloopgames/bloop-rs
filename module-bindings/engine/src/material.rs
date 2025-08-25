use std::{
    ffi::c_char,
    fmt::Display,
    ops::{Deref, DerefMut, Index},
    ptr::slice_from_raw_parts,
};

use bytemuck::{Pod, Zeroable};
use engine_derive::component;
use glam::{Mat4, Vec4};
use log::error;
use serde_big_array::BigArray;

use crate::{
    Component, ComponentId, EcsType, FfiVec,
    graphics::TextureId,
    serialize::{
        default_material_parameters_data, default_material_parameters_material_id,
        default_material_parameters_textures,
    },
};

/// This is a handle identifying a shader template.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Pod, Zeroable)]
pub struct ShaderTemplateId(pub u32);

impl<T> Index<ShaderTemplateId> for [T] {
    type Output = T;

    fn index(&self, index: ShaderTemplateId) -> &Self::Output {
        &self[index.0 as usize]
    }
}

impl Deref for ShaderTemplateId {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ShaderTemplateId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for ShaderTemplateId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for ShaderTemplateId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

/// This is a handle identifying a material.
#[repr(transparent)]
#[derive(
    Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Pod, Zeroable, serde::Deserialize,
)]
pub struct MaterialId(pub u32);

impl Deref for MaterialId {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MaterialId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for MaterialId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for MaterialId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DefaultMaterials {
    Sprite,
    PassThru,
    MissingOrBroken,
}

impl DefaultMaterials {
    pub const fn material_id(&self) -> MaterialId {
        match self {
            DefaultMaterials::Sprite => MaterialId(0),
            DefaultMaterials::PassThru => MaterialId(1),
            DefaultMaterials::MissingOrBroken => MaterialId(2),
        }
    }
}

pub const UNIFORM_LIMIT: usize = 256;
pub const TEXTURE_LIMIT: usize = 16;

#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct MaterialParameters {
    #[serde(default = "default_material_parameters_material_id")]
    material_id: MaterialId,
    #[serde(with = "BigArray", default = "default_material_parameters_data")]
    pub data: [f32; UNIFORM_LIMIT],
    #[serde(default = "default_material_parameters_textures")]
    pub textures: [Option<TextureId>; TEXTURE_LIMIT],
}

impl MaterialParameters {
    pub fn new(material_id: MaterialId) -> MaterialParameters {
        Self {
            material_id,
            data: [0.; UNIFORM_LIMIT],
            textures: [None; TEXTURE_LIMIT],
        }
    }

    pub fn new_with_buffer(material_id: MaterialId, data: [f32; UNIFORM_LIMIT]) -> Self {
        Self {
            material_id,
            data,
            textures: [None; TEXTURE_LIMIT],
        }
    }

    pub fn material_id(&self) -> MaterialId {
        self.material_id
    }

    /// This function is used to create a dynamic uniform buffer static sized
    /// array, which is what the GPU needs for our custom dynamic uniforms per
    /// material that we create. Often we need to prepend this data with a
    /// custom matrix or a custom set of Vec4s. For example, with
    /// [`MaterialType::Sprite`] materials, we need to include the sprite's
    /// transform matrix, color and uv offset as that is how we define our
    /// `DynamicUniform` for those materials. This function allows us to prepend
    /// those dynamically as those needs change.
    pub fn to_uniform_buffer(
        &self,
        matrices: Option<&[Mat4]>,
        vec4s: Option<&[Vec4]>,
    ) -> [f32; UNIFORM_LIMIT] {
        if matrices.is_none() && vec4s.is_none() {
            return self.data;
        }

        let matrix_buffer_length = match matrices {
            Some(matrices) => matrices.len() * 16,
            None => 0,
        };

        let vec4s_buffer_length = match vec4s {
            Some(vec4s) => vec4s.len() * 4,
            None => 0,
        };

        if matrix_buffer_length + vec4s_buffer_length > UNIFORM_LIMIT {
            error!(
                "Attempted to add too many matrices values: {matrix_buffer_length} and/or vec4s values: {vec4s_buffer_length} to the material parameters buffer, limit is {UNIFORM_LIMIT}"
            );
            return self.data;
        }

        let mut buffer = [0.; UNIFORM_LIMIT];
        let mut buffer_slice = buffer.as_mut_slice();

        if let Some(matrices) = matrices {
            for matrix in matrices.iter().take(buffer_slice.len() / 16) {
                buffer_slice[..16].copy_from_slice(&matrix.to_cols_array());
                buffer_slice = &mut buffer_slice[16..];
            }
        };

        if let Some(vec4s) = vec4s {
            for vec4 in vec4s.iter().take(buffer_slice.len() / 4) {
                buffer_slice[..4].copy_from_slice(&vec4.to_array());
                buffer_slice = &mut buffer_slice[4..];
            }
        }

        for data in self.data.iter().take(buffer_slice.len()) {
            buffer_slice[..1].fill(*data);
            buffer_slice = &mut buffer_slice[1..];
        }

        buffer
    }
}

impl Default for MaterialParameters {
    fn default() -> Self {
        Self {
            material_id: DefaultMaterials::Sprite.material_id(),
            data: [0.; UNIFORM_LIMIT],

            // Initialize the entire array with MISSING_TEXTURE_ASSET_ID. Note
            // that the renderer will only bind the ones specified in the
            // material.
            textures: [None; TEXTURE_LIMIT],
        }
    }
}

/// This is intended to be used in C as a representation of the underlying Rust
/// data structure. You must pass this back into a free-ing function from the C
/// API. You must not use `free` in C.
#[repr(C)]
#[derive(Debug)]
pub struct Material {
    pub name: *const c_char,
    pub get_world_offset_body: *const c_char,
    pub get_fragment_color_body: *const c_char,
    pub uniform_types_with_defaults: FfiVec<UniformValue>,
    pub texture_descs: FfiVec<TextureDesc>,
    pub shader_template_id: ShaderTemplateId,
    pub material_id: MaterialId,
}

#[repr(u32)]
#[derive(Copy, Clone, Debug)]
pub enum UniformValueType {
    F32,
    Vec4,
    Array,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub union UniformValueUnion {
    pub f32_value: f32,
    pub vec4: glam::Vec4,
    pub vec4_array: FfiVec<glam::Vec4>,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UniformValue {
    pub uniform_value: UniformValueUnion,
    pub uniform_name: *const c_char,
    pub uniform_value_type: UniformValueType,
}

impl UniformValue {
    pub fn uniform_value_to_string(&self) -> String {
        match &self.uniform_value_type {
            UniformValueType::F32 => unsafe { self.uniform_value.f32_value.to_string() },
            UniformValueType::Vec4 => unsafe { self.uniform_value.vec4.to_string() },
            UniformValueType::Array => {
                let Some(slice) = (unsafe {
                    slice_from_raw_parts(
                        self.uniform_value.vec4_array.ptr,
                        self.uniform_value.vec4_array.len,
                    )
                    .as_ref()
                }) else {
                    return "Uniform Value malformed, could not generate slice for Array variant"
                        .to_string();
                };
                slice
                    .iter()
                    .map(|vec4| vec4.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            }
        }
    }
}

impl std::fmt::Debug for UniformValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let uniform_value_as_string = self.uniform_value_to_string();
        f.debug_struct("UniformValue")
            .field("uniform_name", &self.uniform_name)
            .field("uniform_value_type", &self.uniform_value_type)
            .field("uniform_value", &uniform_value_as_string)
            .finish()
    }
}

impl Display for UniformValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.uniform_value_to_string())
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FilterMode {
    Nearest,
    Linear,
}

#[repr(C)]
#[derive(Debug)]
pub struct TextureDesc {
    pub name: *const c_char,
    pub sampler_filter_mode: FilterMode,
    pub texture_id: Option<TextureId>,
}

#[repr(u32)]
#[derive(Debug)]
pub enum LoadShaderTemplateFromPathResult {
    Success,
    MaterialManagerNull,
    TextAssetManagerNull,
    OutputPendingTextNull,
    ShaderTemplateLoadError,
}

#[repr(u32)]
#[derive(Debug)]
pub enum LoadMaterialFromPathResult {
    Success,
    MaterialManagerNull,
    TextAssetManagerNull,
    OutputPendingTextNull,
    MaterialLoadError,
}

#[repr(u32)]
#[derive(Debug)]
pub enum RegisterMaterialFromStringResult {
    Success,
    MaterialManagerNull,
    OutputMaterialIdNull,
    RegisterMaterialError,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GetIdFromTextIdResult {
    Success,
    MaterialManagerNull,
    OutputMaterialIdNull,
    TextIdNotFound,
}

#[repr(u32)]
#[derive(Debug)]
pub enum UniformNamesDefaultValuesLenResult {
    Success,
    MaterialManagerNull,
    OutputLenNull,
    IdNotFound,
    NoUniforms,
}

#[repr(u32)]
#[derive(Debug)]
pub enum UniformNamesDefaultValuesResult {
    Success,
    MaterialManagerNull,
    IdNotFound,
    InputArrayIncorrectLen,
    NoUniforms,
}

#[repr(u32)]
#[derive(Debug)]
pub enum GenerateShaderTextResult {
    Success,
    OutputStringNull,
    OutputLenNull,
    MaterialManagerNull,
    ErrorGeneratingText,
    ErrorConvertingTextToCString,
}

#[repr(u32)]
#[derive(Debug)]
pub enum UpdateMaterialFromStringResult {
    Success,
    MaterialManagerNull,
    NameAndTomlNull,
    ErrorUpdatingMaterial,
}

#[repr(u32)]
#[derive(Debug)]
pub enum AsUniformValuesLenResult {
    Success,
    MaterialParametersNull,
    MaterialManagerNull,
    OutputLenNull,
    ErrorInAsMaterialUniforms,
}

#[repr(u32)]
#[derive(Debug)]
pub enum AsUniformValuesResult {
    Success,
    MaterialParametersNull,
    MaterialManagerNull,
    OutputUniformValuesNull,
    InputArrayIncorrectLen,
    ErrorInAsMaterialUniforms,
}

#[repr(u32)]
#[derive(Debug)]
pub enum UpdateFromUniformValuesResult {
    Success,
    MaterialParametersNull,
    UpdateFailed,
}

#[repr(u32)]
#[derive(Debug)]
pub enum AsTextureDescsLenResult {
    Success,
    MaterialParametersNull,
    MaterialManagerNull,
    OutputLenNull,
    ErrorInAsTextureDescs,
}

#[repr(u32)]
#[derive(Debug)]
pub enum AsTextureDescsResult {
    Success,
    MaterialParametersNull,
    MaterialManagerNull,
    OutputTextureDescsNull,
    InputArrayIncorrectLen,
    ErrorInAsTextureDescs,
}

#[repr(u32)]
#[derive(Debug)]
pub enum UpdateFromTextureDescsResult {
    Success,
    MaterialParametersNull,
    UpdateFailed,
}

#[repr(u32)]
#[derive(Debug)]
pub enum MaterialsResult {
    Success,
    IncorrectLen,
}
