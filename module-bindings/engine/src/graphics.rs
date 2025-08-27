use std::{
    error::Error,
    ffi::{CStr, c_void},
    fmt::Display,
    num::NonZero,
    ops::{Deref, DerefMut},
    str::{Utf8Error, from_utf8},
};

use engine_derive::component;
use glam::Vec2;
use snapshot_derive::{DeserializeEngine, SerializeEngine};

use crate::{
    AssetId, Component, ComponentId, EcsType,
    serialize::{
        default_circle_render_num_sides, default_color_render_size, default_rect_dimensions,
        default_rect_position, default_text_render_alignment, default_text_render_font_size,
        default_true, deserialize_text_render_text_field,
    },
    text::TextAlignment,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sprite {
    pub position: Vec2,
    pub width: f32,
    pub height: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlendTypes {
    PremultipliedBlendSourceOver,
    Copy,
    UnPremultipliedBlend,
    AdditiveLighten,
    DestinationOver,
    DestinationIn,
    DestinationOut,
    DestinationAtop,
    SourceIn,
    SourceOut,
    SourceAtop,
}

#[repr(C)]
#[derive(Debug, bytemuck::Pod, bytemuck::Zeroable, DeserializeEngine, SerializeEngine)]
#[component(engine)]
pub struct Rect {
    #[serde(default = "default_rect_position")]
    pub position: Vec2,
    #[serde(default = "default_rect_dimensions")]
    pub dimensions: Vec2,
}

impl Rect {
    pub fn new(x: f32, y: f32, w: f32, h: f32) -> Self {
        Rect {
            position: Vec2::new(x, y),
            dimensions: Vec2::new(w, h),
        }
    }
}

impl Default for Rect {
    fn default() -> Self {
        Rect {
            position: Vec2::new(0., 0.),
            dimensions: Vec2::new(1., 1.),
        }
    }
}

#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct TextureRender {
    #[serde(skip_deserializing)]
    pub texture_id: Option<TextureId>,

    // `uv_region` is the normalized uv rectangle when texturing this Sprite.
    // Used for spritesheets, animation, etc.
    #[serde(default)]
    pub uv_region: Rect,

    /// If this is `Some`, this replaces the native texture size as the size of
    /// this sprite. If this is `None`, the native texture size is used.
    #[serde(default)]
    pub size_override: Option<Vec2>,

    #[serde(default = "default_true")]
    pub visible: bool,
}

impl TextureRender {
    pub fn new(texture_id: TextureId) -> Self {
        Self {
            texture_id: Some(texture_id),
            uv_region: Rect::default(),
            size_override: None,
            visible: true,
        }
    }
}

impl Default for TextureRender {
    fn default() -> Self {
        Self {
            texture_id: None,
            uv_region: Rect::default(),
            size_override: None,
            visible: true,
        }
    }
}

#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct ColorRender {
    #[serde(default = "default_color_render_size")]
    pub size: Vec2,
    #[serde(default = "default_true")]
    pub visible: bool,
}

impl Default for ColorRender {
    fn default() -> Self {
        Self {
            size: Vec2::new(32., 32.),
            visible: true,
        }
    }
}

pub(crate) const TEXT_RENDER_SIZE: usize = 256;

#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct TextRender {
    #[serde(deserialize_with = "deserialize_text_render_text_field")]
    pub text: [u8; TEXT_RENDER_SIZE],
    #[serde(default = "default_true")]
    pub visible: bool,
    #[serde(default)]
    pub bounds_size: crate::Vec2,
    #[serde(default = "default_text_render_font_size")]
    pub font_size: f32,
    #[serde(default = "default_text_render_alignment")]
    pub alignment: TextAlignment,
}

impl Default for TextRender {
    fn default() -> Self {
        Self {
            text: [0; TEXT_RENDER_SIZE],
            visible: true,
            bounds_size: Default::default(),
            font_size: 1.0,
            alignment: Default::default(),
        }
    }
}

impl TextRender {
    pub fn new(string: &str, font_size: f32) -> Self {
        Self {
            text: Self::str_to_u8array::<TEXT_RENDER_SIZE>(string),
            font_size,
            ..Default::default()
        }
    }

    pub fn str_to_u8array<const N: usize>(string: &str) -> [u8; N] {
        let mut output_array = [0; N];
        string
            .as_bytes()
            .iter()
            .take(N)
            .enumerate()
            .for_each(|(index, byte)| output_array[index] = *byte);

        output_array
    }

    // TODO: Modify components dealing with text to have a length so we aren't
    // trimming \0s.
    pub fn u8array_to_str(u8_slice: &[u8]) -> Result<&str, Box<dyn Error + Send + Sync>> {
        from_utf8(u8_slice)
            .map(|str| str.trim_matches('\0'))
            .map_err(|err| err.into())
    }

    pub fn get_text(&self) -> Result<&str, Utf8Error> {
        from_utf8(&self.text).map(|str| str.trim_matches('\0'))
    }
}

#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct CircleRender {
    #[serde(default = "default_circle_render_num_sides")]
    pub num_sides: u32,
    #[serde(default = "default_color_render_size")]
    pub size: Vec2,
    #[serde(default = "default_true")]
    pub visible: bool,
}

impl Default for CircleRender {
    fn default() -> Self {
        Self {
            num_sides: 32,
            size: Vec2::new(32., 32.),
            visible: true,
        }
    }
}

impl CircleRender {
    pub fn new(num_sides: u32) -> Self {
        Self {
            num_sides,
            ..Default::default()
        }
    }
}

/// A handle identifying a texture.
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
    DeserializeEngine,
    SerializeEngine,
    serde::Deserialize,
)]
pub struct TextureId(pub NonZero<u32>);

impl TextureId {
    // Reserved TextureIds must match order in
    // [`TextureAssetManager::default()`].
    pub const WHITE: Self = Self(NonZero::new(1).unwrap());
}

impl AsRef<TextureId> for TextureId {
    fn as_ref(&self) -> &TextureId {
        self
    }
}

impl Deref for TextureId {
    type Target = NonZero<u32>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TextureId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for TextureId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<NonZero<u32>> for TextureId {
    fn from(value: NonZero<u32>) -> Self {
        Self(value)
    }
}

pub type ParticleEffectHandle = Option<NonZero<u64>>;

/// A component representing a `ParticleEffect` registered with
/// `ParticleEffectManager`.  An entity can only have one `ParticleRender` at a
/// time.
#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct ParticleRender {
    descriptor_id: AssetId,
    handle: ParticleEffectHandle,
    visible: bool,
}

pub trait ParticleManager {
    fn next_effect_handle(&mut self) -> ParticleEffectHandle;
}

impl ParticleRender {
    pub fn new(descriptor_id: AssetId, visible: bool) -> ParticleRender {
        ParticleRender {
            descriptor_id,
            handle: None,
            visible,
        }
    }

    pub fn handle(&self) -> ParticleEffectHandle {
        self.handle
    }

    pub fn set_visibility(&mut self, new_visibility: bool) {
        self.visible = new_visibility;
    }

    pub fn visible(&self) -> bool {
        self.visible
    }

    pub fn descriptor_id(&self) -> AssetId {
        self.descriptor_id
    }

    /// `init_effect` should only be used by a complete `ParticleManager` (ex:
    /// `GpuWeb::ParticleEffectManager`) that manages simulation and rendering
    /// for this component.  This is a temporary solution until we refactor
    /// `ParticleEffectManager` into more ECS-friendly systems.
    pub fn init_effect<P: ParticleManager>(&mut self, particle_manager: &mut P) {
        self.handle = particle_manager.next_effect_handle();
    }
}

pub struct GpuInterface;

impl GpuInterface {
    pub fn load_texture(&mut self, path: &str) -> TextureId {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _LOAD_TEXTURE_FN.unwrap_unchecked()(
                (self as *mut GpuInterface).cast(),
                path.as_ptr(),
                path.len(),
            )
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            crate::wasm::alloc_and_write_external_slice(path.as_bytes(), |bytes| {
                _LOAD_TEXTURE_FN.unwrap_unchecked()(
                    (self as *mut GpuInterface).cast(),
                    bytes.cast(),
                    path.len(),
                )
            })
        }
    }

    /// Returns true if all the texture IDs in the slice are loaded.
    pub fn all_ids_loaded(&self, texture_ids: &[TextureId]) -> bool {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _ALL_IDS_LOADED_FN.unwrap_unchecked()(
                (self as *const GpuInterface).cast(),
                texture_ids.as_ptr(),
                texture_ids.len(),
            )
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            crate::wasm::alloc_and_write_external_slice(texture_ids, |texture_ids_ptr| {
                _LOAD_TEXTURE_FN.unwrap_unchecked()(
                    (self as *const GpuInterface).cast(),
                    texture_ids_ptr,
                    texture_ids.len(),
                )
            })
        }
    }
}

static mut GPU_INTERFACE_CID: Option<ComponentId> = None;

impl EcsType for GpuInterface {
    fn id() -> ComponentId {
        unsafe { GPU_INTERFACE_CID.expect("ComponentId unassigned") }
    }

    unsafe fn set_id(id: ComponentId) {
        unsafe {
            GPU_INTERFACE_CID = Some(id);
        }
    }

    fn string_id() -> &'static CStr {
        c"game_asset::ecs_module::GpuInterface"
    }
}

pub static mut _LOAD_TEXTURE_FN: Option<
    unsafe extern "C" fn(*mut c_void, *const u8, usize) -> TextureId,
> = None;

pub static mut _ALL_IDS_LOADED_FN: Option<
    unsafe extern "C" fn(*const c_void, *const TextureId, usize) -> bool,
> = None;
