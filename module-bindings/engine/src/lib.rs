//! This library serves two roles.
//!
//! It is the Rust module API to the engine, wrapping the engine's C module API
//! in Rust interfaces. For example, `struct Engine` wraps the callable
//! functions on the engine, while `struct Query` provides an abstraction over
//! querying ECS data.
//!
//! It also contains the type definitions of the `engine` module. These
//! types include common components and resources, such as `Transform` and
//! `InputState`.
//!
//! The Rust module API and the `engine` type definitions will likely be
//! separated into different packages in the future.
//!
//! TODO: Separate Rust module API and the `engine` module.

use std::{
    cmp::Ordering,
    ffi::{CStr, c_char, c_void},
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::MaybeUninit,
    num::NonZero,
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    slice::{self, from_raw_parts},
    time::Duration,
};

use bytemuck::{Pod, Zeroable};
use callable::{AsyncCompletion, Callable};
use engine_derive::{component, resource};
use flatbuffers::{FlatBufferBuilder, Follow, Push, WIPOffset};
pub use glam::{
    Mat2, Mat3, Mat4, Quat, Vec2, Vec3, Vec4,
    swizzles::{Vec2Swizzles, Vec3Swizzles, Vec4Swizzles},
};
pub use log;
pub use serde;
pub use serde_json;
use serialize::{
    default_camera_aspect, default_camera_clear_color, default_camera_projection_matrix,
    default_camera_render_order, default_camera_render_target_texture_id,
    default_camera_view_matrix, default_transform_pivot, default_transform_scale,
};
pub use snapshot;
use snapshot::{Deserialize, Serialize};
use snapshot_derive::{DeserializeEngine, SerializeEngine};
use system::system_name_generator_c;

use crate::{
    colors::Color,
    coordinate_systems::{
        local_to_world, screen_to_clip, screen_to_view, screen_to_world, set_world_position,
        world_position, world_to_clip, world_to_local, world_to_screen, world_to_view,
    },
};

pub mod callable;
pub mod colors;
pub mod coordinate_systems;
pub mod dst;
#[allow(clippy::all, clippy::pedantic, warnings, unused)]
pub mod event;
pub mod graphics;
pub mod input;
pub mod linalg;
pub mod logger;
pub mod material;
pub mod pipeline;
pub mod prelude;
pub mod query;
pub mod rand;
mod serialize;
pub mod system;
pub mod text;

#[macro_export]
macro_rules! concat_bytes {
    ($($s:expr),+) => {{
        $(
            const _: &[u8] = $s; // require constants
        )*
        const LEN: usize = $( $s.len() + )* 0;
        const ARR: [u8; LEN] = {
            use ::std::mem::MaybeUninit;
            let mut arr: [MaybeUninit<u8>; LEN] = [MaybeUninit::zeroed(); LEN];
            let mut base: usize = 0;
            $({
                let mut i = 0;
                while i < $s.len() {
                    arr[base + i] = MaybeUninit::new($s[i]);
                    i += 1;
                }
                base += $s.len();
            })*
            if base != LEN { panic!("invalid length"); }

            unsafe { ::std::mem::transmute(arr) }
        };
        &ARR
    }};
}

/// Returns a `&'static CStr` version of a flatbuffers event name
#[macro_export]
macro_rules! event_name {
    ($event:ident) => {
        unsafe {
            assert!($event::get_fully_qualified_name().is_ascii());
            ::std::ffi::CStr::from_bytes_with_nul_unchecked($crate::concat_bytes!(
                $event::get_fully_qualified_name().as_bytes(),
                &[0]
            ))
        }
    };
}

/// The version of Void which this module is designed to support.
pub const ENGINE_VERSION: u32 = make_api_version(0, 0, 20);

pub const fn make_api_version(major: u32, minor: u32, patch: u32) -> u32 {
    ((major) << 25) | ((minor) << 15) | (patch)
}

pub const fn api_version_major(version: u32) -> u32 {
    version >> 25
}

pub const fn api_version_minor(version: u32) -> u32 {
    (version >> 15) & !(!0 << 10)
}

pub const fn api_version_patch(version: u32) -> u32 {
    version & !(!0 << 15)
}

pub const fn api_version_compatible(version: u32) -> bool {
    api_version_major(ENGINE_VERSION) == api_version_major(version)
        && api_version_minor(ENGINE_VERSION) == api_version_minor(version)
        // consider patch version breaking until public release
        && api_version_patch(ENGINE_VERSION) == api_version_patch(version)
}

/// A handle identifying a component or resource type.
pub type ComponentId = NonZero<u16>;

/// A handle identifying a loaded asset.
#[repr(transparent)]
#[derive(
    Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Pod, Zeroable, serde::Deserialize,
)]
pub struct AssetId(pub u32);

impl Deref for AssetId {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for AssetId {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for AssetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for AssetId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

pub trait EcsType {
    fn id() -> ComponentId;

    /// # Safety
    ///
    /// This function sets a global static. The caller must ensure that no race
    /// conditions occur.
    unsafe fn set_id(id: ComponentId);

    fn string_id() -> &'static CStr;
}

/// A trait representing an ECS Component. All structs which are to be used as
/// a Component must `#[derive(Component)]`.
pub trait Component: EcsType + Copy + Clone + Send + Sync + Sized + 'static {}

/// A trait representing an ECS Resource. All structs which are to be used as
/// a Resource must `#[derive(Resource)]`.
pub trait Resource: EcsType + Send + Sync + Sized + Serialize + Deserialize + 'static {
    fn new() -> Self;
}

/// A repr(C) compatible wrapper of Option<T>. T must implement [`Copy`]. If you
/// need [`FfiOption`] on a non [`Copy`] type, perhaps consider an alternative
/// first. Because [`Drop`] and [`Copy`] are mutually exclusive traits, it is
/// impossible to implement [`Drop`] on this [`FfiOption`], and recreating a version
/// of this struct that can implement [`Drop`] opens a risk of creating memory
/// leaks.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FfiOption<T: Copy> {
    val: MaybeUninit<T>,
    is_some: bool,
}

impl<T: Copy> FfiOption<T> {
    pub fn new(val: Option<T>) -> Self {
        Self {
            is_some: val.is_some(),
            val: val.map_or(MaybeUninit::uninit(), |value| MaybeUninit::new(value)),
        }
    }

    pub fn unwrap(self) -> T {
        match self.is_some {
            true => unsafe { self.val.assume_init_read() },
            false => panic!("called `FfiOption::unwrap()` on a `None` value"),
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self.is_some {
            true => unsafe { self.val.assume_init_read() },
            false => default,
        }
    }

    pub fn borrow(&self) -> Option<&T> {
        match self.is_some {
            true => Some(unsafe { self.val.assume_init_ref() }),
            false => None,
        }
    }

    pub fn borrow_mut(&mut self) -> Option<&mut T> {
        match self.is_some {
            true => Some(unsafe { self.val.assume_init_mut() }),
            false => None,
        }
    }

    pub fn set(&mut self, val: Option<T>) {
        self.is_some = val.is_some();
        self.val = val.map_or(MaybeUninit::uninit(), |value| MaybeUninit::new(value));
    }
}

impl<T: Copy> From<Option<T>> for FfiOption<T> {
    fn from(value: Option<T>) -> Self {
        Self::new(value)
    }
}

impl<T: Copy + Display> Display for FfiOption<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_some {
            write!(f, "FfiSome({})", unsafe { self.val.assume_init_ref() })
        } else {
            write!(f, "FfiNone")
        }
    }
}

impl<T: Copy + PartialEq> PartialEq for FfiOption<T> {
    fn eq(&self, other: &Self) -> bool {
        if !self.is_some && !other.is_some {
            // if both are None we are equal
            return true;
        }

        if !self.is_some || !other.is_some {
            // if one is None and the other is Some, we are not equal
            return false;
        }

        if self.is_some && other.is_some {
            unsafe { return self.val.assume_init_ref().eq(other.val.assume_init_ref()) }
        }

        false
    }
}

impl<T: Copy + PartialEq + Eq> Eq for FfiOption<T> {}

impl<T: Copy + Hash> Hash for FfiOption<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.is_some {
            unsafe { self.val.assume_init_ref().hash(state) };
        } else {
            self.is_some.hash(state);
        }
    }
}

impl<T: Copy + PartialOrd> PartialOrd for FfiOption<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if !self.is_some && !other.is_some {
            // if both are None we are equal
            return Some(Ordering::Equal);
        }

        if !self.is_some || !other.is_some {
            // if one is None and the other is not, we are not equal
            return None;
        }

        if self.is_some && other.is_some {
            unsafe {
                return self
                    .val
                    .assume_init_ref()
                    .partial_cmp(self.val.assume_init_ref());
            }
        }

        None
    }
}

/// A handle representing an entity.
#[repr(transparent)]
#[derive(Debug, Hash, PartialEq, Eq, DeserializeEngine, SerializeEngine)]
#[component(engine)]
pub struct EntityId(NonZero<u64>);

impl EntityId {
    pub fn new(value: NonZero<u64>) -> Self {
        Self(value)
    }
}

/// A resource containing values which are constant for the whole frame.
#[repr(C)]
#[derive(Default, Clone, Copy)]
#[resource(engine)]
pub struct FrameConstants {
    /// The time since the previous frame, in seconds.
    pub delta_time: f32,

    /// The averaged inverse of `delta_time`. This is mostly only useful for
    /// diagnosics.
    pub frame_rate: f32,

    /// The number of frames since the game started.
    pub frames_since_boot: u64,

    /// The total time since the game started, in tenths of milliseconds (i.e.
    /// `1 / 10_000` seconds).
    ///
    /// Callers should prefer to use `duration_since_boot()`, which wraps this
    /// value in `std::time::Duration`.
    pub time_since_boot_tenths_of_millis: u64,
}

impl FrameConstants {
    /// Returns `time_since_boot_tenths_of_millis` as `std::time::Duration`,
    /// which will typically be easier to work with.
    pub fn duration_since_boot(&self) -> Duration {
        Duration::from_micros(self.time_since_boot_tenths_of_millis * 100)
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
#[resource(engine)]
pub struct FrameConfig {
    /// The frame delta time will be clamped to this maximum (seconds)
    pub max_delta_time: f32,
}

impl Default for FrameConfig {
    fn default() -> Self {
        Self {
            max_delta_time: 0.4,
        }
    }
}

/// A component representing a 3D transform.
#[repr(C)]
#[derive(Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[component(engine)]
pub struct Transform {
    #[serde(default)]
    pub position: linalg::Vec3,
    #[serde(default = "default_transform_scale")]
    pub scale: linalg::Vec2,
    #[serde(default)]
    pub skew: linalg::Vec2,
    #[serde(default = "default_transform_pivot")]
    pub pivot: linalg::Vec2,
    #[serde(default)]
    pub rotation: f32,
    #[serde(skip)]
    pub _padding: f32,
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            position: Default::default(),
            rotation: 0.0,
            scale: Vec2::ONE.into(),
            skew: Vec2::ZERO.into(),
            pivot: Vec2::splat(0.5).into(),
            _padding: 0.,
        }
    }
}

impl Transform {
    pub fn new(position: Vec3) -> Self {
        Self {
            position: position.into(),
            ..Default::default()
        }
    }

    pub fn from_scale_rotation_translation(
        scale: &Vec2,
        rotation: f32,
        translation: &Vec2,
    ) -> Self {
        Self {
            position: translation.extend(0.0).into(),
            scale: (*scale).into(),
            rotation,
            ..Default::default()
        }
    }

    pub fn from_rotation_translation(rotation: f32, translation: &Vec2) -> Self {
        Self {
            position: translation.extend(0.0).into(),
            rotation,
            ..Default::default()
        }
    }

    pub fn from_translation(translation: &Vec2) -> Self {
        Self {
            position: translation.extend(0.0).into(),
            ..Default::default()
        }
    }

    /// Set the world-space position of this `Transform` using the given `local_to_world` matrix
    pub fn set_world_position(&mut self, world_position: &Vec2, local_to_world: &LocalToWorld) {
        set_world_position(self, local_to_world, world_position);
    }
}

#[repr(C)]
#[derive(Debug, bytemuck::Pod, bytemuck::Zeroable)]
#[component(engine)]
pub struct LocalToWorld(linalg::Mat4);

impl Default for LocalToWorld {
    fn default() -> Self {
        Self(linalg::Mat4::new(Mat4::IDENTITY))
    }
}

impl Display for LocalToWorld {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in 0..4 {
            writeln!(f, "{}", self.model_matrix().row(row as usize))?;
        }
        Ok(())
    }
}

impl From<linalg::Mat4> for LocalToWorld {
    fn from(value: linalg::Mat4) -> Self {
        Self(value)
    }
}

impl From<LocalToWorld> for linalg::Mat4 {
    fn from(value: LocalToWorld) -> Self {
        value.0
    }
}

impl From<Mat4> for LocalToWorld {
    fn from(value: Mat4) -> Self {
        Self(linalg::Mat4::from(value))
    }
}

impl From<LocalToWorld> for Mat4 {
    fn from(value: LocalToWorld) -> Self {
        *value.0
    }
}

impl LocalToWorld {
    #[inline]
    pub fn model_matrix(&self) -> &Mat4 {
        &self.0
    }

    /// Convert the given `world-position` into the local coordinate of this entity.
    #[inline]
    pub fn world_to_local(&self, world_position: &Vec2) -> Vec2 {
        world_to_local(world_position, self)
    }

    /// Convert the given `local_position` of this entity into world-space.
    #[inline]
    pub fn local_to_world(&self, local_position: &Vec2) -> Vec2 {
        local_to_world(local_position, self)
    }

    /// Returns the world-space position of this entity.
    #[inline]
    pub fn world_position(&self) -> Vec2 {
        world_position(self)
    }
}

#[derive(Debug, Copy, Clone, serde::Deserialize)]
pub struct Viewport {
    /// A normalized value indicating the start x position of the viewport relative to the window.
    pub x: f32,
    /// A normalized value indicating the start y position of the viewport relative to the window.
    pub y: f32,
    /// A normalized value indicating the percentage of the window width this viewport represents.
    pub width: f32,
    /// A normalized value indicating the percentage of the window height this viewport represents.
    pub height: f32,
}

impl Default for Viewport {
    fn default() -> Self {
        Self {
            x: 0.0,
            y: 0.0,
            width: 1.0,
            height: 1.0,
        }
    }
}

impl Viewport {
    pub fn new(x: f32, y: f32, width: f32, height: f32) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }
}

/// A component representing a 3D camera.
#[repr(C)]
#[derive(Debug)]
#[component(engine)]
pub struct Camera {
    /// The aspect of the camera controls how the game is rendered relative to
    /// the window size.
    #[serde(default = "default_camera_aspect")]
    pub aspect: CameraAspect,

    /// The clear color of this camera.
    #[serde(default = "default_camera_clear_color")]
    pub clear_color: Color,

    /// The normalized area of the viewport that this camera will render to.
    /// This is useful for things like splitscreen rendering.
    //
    //     |----------------| 1,1
    //     |                |
    //     |                |
    //     |                |
    // 0,0 |----------------|
    #[serde(default)]
    pub viewport_area: Viewport,

    /// The camera's render target texture id if assigned. If `None` will render to `ColorMSAA`.
    #[serde(default = "default_camera_render_target_texture_id")]
    pub render_target_texture_id: FfiOption<u32>,

    /// The render priority for this camera. Cameras with a higher `render_order` will be rendered
    /// first.
    #[serde(default = "default_camera_render_order")]
    pub render_order: i32,

    /// The matrix that converts from world-space to camera space (view-space).
    /// Intended for internal use only.
    #[serde(default = "default_camera_view_matrix")]
    pub __view_matrix: linalg::Mat4,

    /// The matrix that converts from camera-space into homogenous clip space.
    /// Intended for internal use only.
    #[serde(default = "default_camera_projection_matrix")]
    pub __projection_matrix: linalg::Mat4,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, serde::Deserialize)]
pub enum CameraAspect {
    /// There are no constraints on the camera. One world unit will equal one
    /// physical on-screen pixel. For example, a game window of 640x480 will
    /// render 640x480 world units.
    Freeform,

    /// The specified `width`, in world units, will scale to fit the window's
    /// width. The amount of world units shown on the Y-axis will vary as the
    /// window's width is changed, to ensure that no stretching occurs.
    FixedVirtualWidth { width: f32 },

    /// The specified `height`, in world units, will scale to fit the window's
    /// height. The amount of world units shown on the X-axis will vary as the
    /// window's height is changed, to ensure that no stretching occurs.
    FixedVirtualHeight { height: f32 },
}

impl Default for Camera {
    fn default() -> Self {
        Self {
            aspect: CameraAspect::Freeform,
            clear_color: Color::BLACK,
            viewport_area: Viewport::default(),
            render_target_texture_id: FfiOption::new(None),
            render_order: 0,
            __view_matrix: linalg::Mat4::new(Mat4::IDENTITY),
            __projection_matrix: linalg::Mat4::new(Mat4::IDENTITY),
        }
    }
}

impl Camera {
    /// Convert the given world-space `position` into the local coordinate system of this `Camera`
    pub fn world_to_view(&self, position: &Vec3) -> Vec3 {
        world_to_view(position, self)
    }

    /// Convert the given world-space `position` into the homogenous clip space this `Camera`
    pub fn world_to_clip(&self, position: &Vec3) -> Vec3 {
        world_to_clip(position, self)
    }

    /// Convert the given world-space `position` into the screen space of this `Camera` at the given
    /// `screen_dimensions` in pixels.
    pub fn world_to_screen(&self, position: &Vec3, screen_dimensions: &Vec2) -> Vec2 {
        world_to_screen(position, screen_dimensions, self)
    }

    /// Convert the given screen-space `screen_position` of this `Camera` and `screen_dimensions`
    /// into a world-space position.
    pub fn screen_to_world(&self, screen_position: &Vec2, screen_dimensions: &Vec2) -> Vec3 {
        screen_to_world(screen_position, screen_dimensions, self)
    }

    /// Convert the given screen-space `screen_position` using the given `screen_dimensions` into its
    /// homogenous clip space equivalent.
    pub fn screen_to_clip(&self, screen_position: &Vec2, screen_dimensions: &Vec2) -> Vec3 {
        screen_to_clip(screen_position, screen_dimensions)
    }

    /// Convert the given screen-space `screen_position` using the `screen_dimensions`
    /// into the local coordinate system of this `Camera`.
    pub fn screen_to_view(&self, screen_position: &Vec2, screen_dimensions: &Vec2) -> Vec3 {
        screen_to_view(screen_position, screen_dimensions, self)
    }
}

/// A resource representing the game window size (in pixels).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
#[resource(engine)]
pub struct Aspect {
    pub width: f32,
    pub height: f32,
}

/// A generic storage for data from any component type. This type is used to be
/// able to collect various types of component data at runtime to eventually
/// transform to [`ComponentRef`] and pass into [`Engine::spawn`].
///
/// It is NOT recommend to use this struct manually -- use the
/// [`ComponentBuilder`] to automatically collect components.
#[repr(C)]
#[derive(Debug)]
pub struct ComponentData {
    /// `component_id` is an `Option` to enforce an explicit check which ensures
    /// that the component ids is valid and non-zero.
    component_id: Option<ComponentId>,
    component_data: Vec<MaybeUninit<u8>>,
}

impl ComponentData {
    pub fn new(component_id: ComponentId, component_data: Vec<MaybeUninit<u8>>) -> Self {
        Self {
            component_id: Some(component_id),
            component_data,
        }
    }

    pub fn component_id(&self) -> Option<ComponentId> {
        self.component_id
    }
}

impl<C: Component> From<C> for ComponentData {
    fn from(value: C) -> Self {
        let ptr = (&value as *const C).cast::<MaybeUninit<u8>>();
        let size = size_of::<C>();

        Self {
            component_id: Some(C::id()),
            component_data: unsafe { from_raw_parts(ptr, size).to_vec() },
        }
    }
}

/// A generic reference to a component. This type is necessary to pass
/// components to `Engine::spawn()`.
///
/// It is NOT recommended to use this struct manually -- use the `bundle!()`
/// macro to automatically convert components.
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct ComponentRef<'a> {
    /// `component_id` is an `Option` to enforce an explicit check which ensures
    /// that the component ids is valid and non-zero.
    pub component_id: Option<ComponentId>,
    pub component_size: usize,
    pub component_val: *const c_void,
    marker: PhantomData<&'a MaybeUninit<u8>>,
}

impl ComponentRef<'_> {
    pub fn new(component_id: ComponentId, component_val: &[MaybeUninit<u8>]) -> Self {
        Self {
            component_id: Some(component_id),
            component_size: component_val.len(),
            component_val: component_val.as_ptr().cast::<c_void>(),
            marker: PhantomData,
        }
    }
}

impl<'a, C> From<&'a C> for ComponentRef<'a>
where
    C: Component,
{
    fn from(value: &'a C) -> Self {
        Self {
            component_id: Some(C::id()),
            component_size: size_of::<C>(),
            component_val: (value as *const C).cast::<c_void>(),
            marker: PhantomData,
        }
    }
}

impl<'a, C> From<&'a mut C> for ComponentRef<'a>
where
    C: Component,
{
    fn from(value: &'a mut C) -> Self {
        (&*value).into()
    }
}

impl<'a> From<&'a ComponentData> for ComponentRef<'a> {
    fn from(value: &ComponentData) -> Self {
        Self {
            component_id: value.component_id,
            component_size: value.component_data.len(),
            component_val: value.component_data.as_ptr().cast(),
            marker: PhantomData,
        }
    }
}

/// Converts a list of component references into the type which `Engine::spawn()` accepts.
#[macro_export]
macro_rules! bundle {
    ($($c:expr),* $(,)?) => (
        &[$(::engine::ComponentRef::from($c)),*]
    )
}

/// Converts a list of component references into a type which [`ComponentBuilder`] uses
#[macro_export]
macro_rules! bundle_for_builder {
    ($($c:expr),* $(,)?) => {
        [$(::engine::ComponentData::from($c)),*]
    };
}

/// This is a convenience struct for constructing runtime [`Component`]
/// collections of type erased [`Component`] data. This is only recommended for
/// cases where you must create dynamic components at runtime, as this creates
/// an unnecessary allocation that just using `Engine::spawn(bundles!(/*
/// components here */))` does not. It is recommended that you construct this
/// struct with the `bundle_for_builder` macro, like this...
///
/// # Example
///
/// ```ignore
/// use engine::{AssetId, bundle_for_builder, colors::palette, ComponentBuilder, graphics::TextureRender, Transform};
///
/// let component_builder: ComponentBuilder = bundle_for_builder!(Transform::default(), palette::RED, TextureRender { asset_id: AssetId(0), visible: true}).into();
/// ```
#[derive(Debug)]
pub struct ComponentBuilder(Vec<ComponentData>);

impl<const N: usize> From<[ComponentData; N]> for ComponentBuilder {
    fn from(value: [ComponentData; N]) -> Self {
        Self(value.into())
    }
}

impl ComponentBuilder {
    pub fn add_component<C: Component>(&mut self, component: C) {
        self.0.push(component.into());
    }

    /// Add multiple components to an existing builder. It is strongly
    /// recommended that you use the [`bundle_for_builder`] macro with this
    /// method, like this...
    ///
    /// # Example
    ///
    /// ```ignore
    /// use engine::{AssetId, bundle_for_builder, colors::palette, ComponentBuilder, graphics::TextureRender, Transform};
    ///
    /// let mut component_builder: ComponentBuilder = bundle_for_builder!(TextureRender { asset_id: AssetId(0), visible: true}).into();
    /// component_builder.add_components(bundle_for_builder!(Transform::default(), palette::RED));
    /// ```
    pub fn add_components<const N: usize>(&mut self, components: [ComponentData; N]) {
        self.0.extend(components);
    }

    /// Outputs the [`ComponentRef`]s for the underlying [`ComponentData`] in
    /// the builder, likely will be used with [`Engine::spawn`]
    ///
    /// # Example
    ///
    /// ```ignore
    /// Engine::spawn(component_builder.component_refs().as_slice());
    /// ```
    pub fn build(&self) -> Vec<ComponentRef<'_>> {
        self.0.iter().map(ComponentRef::from).collect()
    }
}

pub struct Engine;

impl Engine {
    /// Loads a string containing scene data into the engine
    pub fn load_scene(scene_str: &CStr) {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _LOAD_SCENE.unwrap_unchecked()(scene_str.as_ptr());
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_and_write_external_slice(scene_str.to_bytes_with_nul(), |scene_str| {
                _LOAD_SCENE.unwrap_unchecked()(scene_str.cast());
            });
        }
    }

    /// Spawns an entity with the specified components.
    ///
    /// Returns the `EntityId` of the new entity.
    ///
    /// NOTE: commands are deferred until the end of the frame, so the spawned
    /// entity will not be iterated by queries on the frame it is spawned.
    pub fn spawn(components: &[ComponentRef<'_>]) -> EntityId {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _SPAWN.unwrap_unchecked()(components.as_ptr(), components.len())
                .expect("could not spawn entity")
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_components_external(components, |components| {
                _SPAWN.unwrap_unchecked()(components.as_ptr(), components.len())
                    .expect("could not spawn entity")
            })
        }
    }

    /// Despawns an entity with the specified `EntityId`.
    ///
    /// NOTE: commands are deferred until the end of the frame, so the despawned
    /// entity will still be iterated by queries on the frame it is despawned.
    pub fn despawn(entity_id: EntityId) {
        unsafe {
            _DESPAWN.unwrap_unchecked()(entity_id);
        }
    }

    /// Adds components to an existing entity.
    ///
    /// NOTE: commands are deferred until the end of the frame, so the new
    /// components will not be iterated by queries on the frame that they are
    /// added.
    pub fn add_components(entity_id: EntityId, components: &[ComponentRef<'_>]) {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _ADD_COMPONENTS_FN.unwrap_unchecked()(entity_id, components.as_ptr(), components.len());
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_components_external(components, |components| {
                _ADD_COMPONENTS_FN.unwrap_unchecked()(
                    entity_id,
                    components.as_ptr(),
                    components.len(),
                );
            });
        }
    }

    /// Removes components from an existing entity.
    ///
    /// NOTE: commands are deferred until the end of the frame, so the removed
    /// components will still be iterated by queries on the frame that they are
    /// removed.
    pub fn remove_components(entity_id: EntityId, component_ids: &[ComponentId]) {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _REMOVE_COMPONENTS_FN.unwrap_unchecked()(
                entity_id,
                component_ids.as_ptr(),
                component_ids.len(),
            );
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_and_write_external_slice(component_ids, |ptr| {
                _REMOVE_COMPONENTS_FN.unwrap_unchecked()(
                    entity_id,
                    ptr.cast(),
                    component_ids.len(),
                );
            });
        }
    }

    /// Returns the label associated with an entity via the provided closure.
    /// If no label is associated with the entity, `None` is passed to the
    /// closure. The closure is always run, even if no label is associated with
    /// the entity.
    ///
    /// This function returns a value via a closure rather than a direct return
    /// type, because the lifetime of the label is not guaranteed to be valid
    /// indefinitely.
    pub fn entity_label<F, R>(entity_id: EntityId, f: F) -> R
    where
        F: FnOnce(Option<&CStr>) -> R,
    {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            let ptr = _ENTITY_LABEL_FN.unwrap_unchecked()(entity_id);

            let label = if ptr.is_null() {
                None
            } else {
                Some(CStr::from_ptr(ptr))
            };

            f(label)
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            let ptr = _ENTITY_LABEL_FN.unwrap_unchecked()(entity_id);
            if !ptr.is_null() {
                let len = wasm::wasm_read_external_cstr_len(ptr);

                let mut bytes = Vec::<u8>::with_capacity(len + 1);
                wasm::wasm_read_external_data(bytes.as_mut_ptr().cast(), ptr.cast(), len);
                bytes.spare_capacity_mut()[len].write(0);
                bytes.set_len(len + 1);

                f(Some(CStr::from_bytes_with_nul_unchecked(&bytes)))
            } else {
                f(None)
            }
        }
    }

    /// Associates an entity with a label. An entity may only have one label,
    /// and all labels must be unique (entities cannot share identical labels).
    pub fn set_entity_label(entity_id: EntityId, label: &CStr) {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _SET_ENTITY_LABEL_FN.unwrap_unchecked()(entity_id, label.as_ptr());
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            crate::wasm::alloc_and_write_external_slice(label.to_bytes_with_nul(), |label| {
                _SET_ENTITY_LABEL_FN.unwrap_unchecked()(entity_id, label.cast());
            });
        }
    }

    /// Clears an entity's label.
    pub fn clear_entity_label(entity_id: EntityId) {
        unsafe {
            _SET_ENTITY_LABEL_FN.unwrap_unchecked()(entity_id, ptr::null());
        }
    }

    pub fn call<'a, F: Callable>(parameters: impl Into<F::Parameters<'a>>)
    where
        F::Parameters<'a>: Push,
    {
        let mut builder = FlatBufferBuilder::new();
        let parameters = builder.push(parameters.into());
        builder.finish_minimal(parameters);
        let parameter_data = builder.finished_data();

        unsafe {
            _CALL_FN.unwrap_unchecked()(
                F::id(),
                parameter_data.as_ptr().cast(),
                parameter_data.len(),
            );
        }
    }

    pub fn call_with_builder<'a, F: Callable>(
        parameters: impl FnOnce(&mut FlatBufferBuilder<'a>) -> WIPOffset<F::Parameters<'a>>,
    ) {
        let mut builder = FlatBufferBuilder::new();
        let parameters = parameters(&mut builder);
        builder.finish_minimal(parameters);
        let parameter_data = builder.finished_data();

        unsafe {
            _CALL_FN.unwrap_unchecked()(
                F::id(),
                parameter_data.as_ptr().cast(),
                parameter_data.len(),
            );
        }
    }

    pub fn call_async<'a, F: AsyncCompletion>(
        parameters: impl Into<<F::Function as Callable>::Parameters<'a>>,
        user_data: impl Into<F::UserData<'a>>,
    ) where
        <F::Function as Callable>::Parameters<'a>: Push,
        F::UserData<'a>: Push,
    {
        let mut builder = FlatBufferBuilder::new();
        let parameters = builder.push(parameters.into());
        builder.finish_minimal(parameters);
        let parameter_data = builder.finished_data();

        let mut builder = FlatBufferBuilder::new();
        let user_data = builder.push(user_data.into());
        builder.finish_minimal(user_data);
        let user_data = builder.finished_data();

        unsafe {
            _CALL_ASYNC_FN.unwrap_unchecked()(
                F::id(),
                parameter_data.as_ptr().cast(),
                parameter_data.len(),
                user_data.as_ptr().cast(),
                user_data.len(),
            );
        }
    }

    pub fn call_async_with_builder<'a, F: AsyncCompletion>(
        parameters: impl FnOnce(
            &mut FlatBufferBuilder<'a>,
        ) -> WIPOffset<<F::Function as Callable>::Parameters<'a>>,
        user_data: impl FnOnce(&mut FlatBufferBuilder<'a>) -> WIPOffset<F::UserData<'a>>,
    ) {
        let mut builder = FlatBufferBuilder::new();
        let parameters = parameters(&mut builder);
        builder.finish_minimal(parameters);
        let parameter_data = builder.finished_data();

        let mut builder = FlatBufferBuilder::new();
        let user_data = user_data(&mut builder);
        builder.finish_minimal(user_data);
        let user_data = builder.finished_data();

        unsafe {
            _CALL_ASYNC_FN.unwrap_unchecked()(
                F::id(),
                parameter_data.as_ptr().cast(),
                parameter_data.len(),
                user_data.as_ptr().cast(),
                user_data.len(),
            );
        }
    }

    /// Assign a new parent to the given entity
    ///
    /// This assignment is deferred until the next data sync point (currently the end of cpu system updates).
    /// Until that point, the entity's parent will be unchanged.
    pub fn set_parent_deferred(
        entity_id: EntityId,
        parent_data: Option<EntityId>,
        keep_world_space_transform: bool,
    ) {
        unsafe {
            _SET_PARENT_FN.unwrap_unchecked()(entity_id, parent_data, keep_world_space_transform);
        }
    }

    /// Get the parent id of the given entity. If the requested entity has no parent,
    /// (or if the given entity doesn't exist) this function will return `None`.
    pub fn get_parent(entity_id: EntityId) -> Option<EntityId> {
        let mut out_parent_id = MaybeUninit::<Option<EntityId>>::uninit();

        #[cfg(not(feature = "dynamic_wasm"))]
        let res = unsafe { _GET_PARENT_FN.unwrap_unchecked()(entity_id, &mut out_parent_id) };

        #[cfg(feature = "dynamic_wasm")]
        let res = unsafe {
            wasm::alloc_and_read_external(&mut out_parent_id, |out_parent_id| {
                _GET_PARENT_FN.unwrap_unchecked()(entity_id, out_parent_id)
            })
        };

        if res {
            unsafe { out_parent_id.assume_init() }
        } else {
            None
        }
    }

    fn set_fully_qualified_system_enabled(fully_qualified_system_name: &CStr, enabled: bool) {
        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _SET_SYSTEM_ENABLED_FN.unwrap_unchecked()(
                fully_qualified_system_name.as_ptr(),
                enabled,
            );
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_and_write_external_slice(
                fully_qualified_system_name.to_bytes_with_nul(),
                |name| {
                    _SET_SYSTEM_ENABLED_FN.unwrap_unchecked()(name.cast(), enabled);
                },
            );
        }
    }

    /// Allows a system to be turned off or on. The `set_system_enabled` macro
    /// is generally preferred, but that only accepts system functions as
    /// parameters, so if you need to pass in the system name by it's &`CStr`
    /// value, this is the correct API.
    pub fn set_system_enabled(
        system_name: &CStr,
        enabled: bool,
        module_name_function: ModuleNameFn,
    ) {
        Self::set_fully_qualified_system_enabled(
            &system_name_generator_c(
                unsafe { CStr::from_ptr(module_name_function()) },
                system_name,
            ),
            enabled,
        );
    }
}

pub type ModuleNameFn = unsafe extern "C" fn() -> *const c_char;

pub struct EventReader<T> {
    handle: *const c_void,
    marker: PhantomData<T>,
}

unsafe impl<T> Send for EventReader<T> {}
unsafe impl<T> Sync for EventReader<T> {}

impl<'a, T: Follow<'a> + 'a> EventReader<T> {
    /// # Safety
    ///
    /// `EventReader` should only be constructed from a valid pointer retrieved
    /// from a corresponding `EventReader` parameter in an ECS system's FFI function.
    pub unsafe fn new(handle: *const c_void) -> Self {
        Self {
            handle,
            marker: PhantomData,
        }
    }

    pub fn get(&self, index: usize) -> Option<EventRef<'a, T>> {
        unsafe {
            let ptr = _EVENT_GET_FN.unwrap_unchecked()(self.handle, index);
            ptr.map(|ptr| EventRef::new(ptr))
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = EventRef<'a, T>> {
        let count = unsafe { _EVENT_COUNT_FN.unwrap_unchecked()(self.handle) };

        (0..count).map(|i| unsafe {
            let ptr = _EVENT_GET_FN.unwrap_unchecked()(self.handle, i).unwrap_unchecked();
            EventRef::new(ptr)
        })
    }
}

impl<'a, T: Follow<'a> + 'a> IntoIterator for &'a EventReader<T> {
    type Item = EventRef<'a, T>;

    type IntoIter = EventReaderIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        EventReaderIter { i: 0, reader: self }
    }
}

pub struct EventReaderIter<'a, T> {
    i: usize,
    reader: &'a EventReader<T>,
}

impl<'a, T: Follow<'a> + 'a> Iterator for EventReaderIter<'a, T> {
    type Item = EventRef<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(val) = self.reader.get(self.i) {
            self.i += 1;
            Some(val)
        } else {
            None
        }
    }
}

pub struct EventRef<'a, T: Follow<'a>> {
    inner: T::Inner,

    #[cfg(feature = "dynamic_wasm")]
    _buffer: std::pin::Pin<Box<[u8]>>,
}

impl<'a, T: Follow<'a> + 'a> EventRef<'a, T> {
    #[cfg(not(feature = "dynamic_wasm"))]
    unsafe fn new(ptr: NonNull<u64>) -> Self {
        let ptr = ptr.as_ptr();

        let inner = unsafe {
            let len = ptr.read() as usize;
            // data immediately follows `len`, offset by 1 to get data
            let data = slice::from_raw_parts(ptr.offset(1).cast(), len);
            flatbuffers::root_unchecked::<T>(data)
        };

        Self { inner }
    }

    #[cfg(feature = "dynamic_wasm")]
    unsafe fn new(ptr: NonNull<u64>) -> Self {
        let ptr = ptr.as_ptr();

        unsafe {
            // Read event byte length.
            let mut len = MaybeUninit::<u64>::uninit();
            wasm::wasm_read_external_data(len.as_mut_ptr().cast(), ptr.cast(), size_of::<u64>());
            let len = len.assume_init() as usize;

            // Copy event data into module memory.
            let mut buffer = Vec::with_capacity(len);
            wasm::wasm_read_external_data(
                buffer.spare_capacity_mut().as_mut_ptr().cast(),
                ptr.offset(1).cast(),
                len,
            );
            buffer.set_len(len);

            let buffer = Box::into_pin(buffer.into_boxed_slice());
            let data = slice::from_raw_parts(buffer.as_ptr(), len);
            let inner = flatbuffers::root_unchecked::<T>(data);

            Self {
                inner,
                _buffer: buffer,
            }
        }
    }
}

impl<'a, T: Follow<'a> + 'a> Deref for EventRef<'a, T> {
    type Target = T::Inner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, T: Follow<'a> + 'a> Debug for EventRef<'a, T>
where
    T::Inner: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

pub struct EventWriter<T> {
    handle: *const c_void,
    marker: PhantomData<T>,
}

unsafe impl<T> Send for EventWriter<T> {}
unsafe impl<T> Sync for EventWriter<T> {}

impl<T> EventWriter<T> {
    /// # Safety
    ///
    /// `EventWriter` should only be constructed from a valid pointer retrieved
    /// from a corresponding `EventWriter` parameter in an ECS system's FFI function.
    pub unsafe fn new(handle: *const c_void) -> Self {
        Self {
            handle,
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn write(&self, event: T)
    where
        T: Push,
    {
        let mut builder = flatbuffers::FlatBufferBuilder::new();
        let event = builder.push(event);
        builder.finish_minimal(event);

        let data = builder.finished_data();

        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _EVENT_SEND_FN.unwrap_unchecked()(self.handle, data.as_ptr(), data.len());
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_and_write_external_slice(data, |ptr| {
                _EVENT_SEND_FN.unwrap_unchecked()(self.handle, ptr.cast(), data.len());
            });
        }
    }

    pub fn write_builder<'a, F>(&self, f: F)
    where
        T: Follow<'a>,
        F: FnOnce(&mut FlatBufferBuilder<'a>) -> WIPOffset<T>,
    {
        let mut builder = flatbuffers::FlatBufferBuilder::new();
        let val = f(&mut builder);
        builder.finish_minimal(val);

        let data = builder.finished_data();

        #[cfg(not(feature = "dynamic_wasm"))]
        unsafe {
            _EVENT_SEND_FN.unwrap_unchecked()(self.handle, data.as_ptr(), data.len());
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            wasm::alloc_and_write_external_slice(data, |ptr| {
                _EVENT_SEND_FN.unwrap_unchecked()(self.handle, ptr.cast(), data.len());
            });
        }
    }
}

/// An immutable Component or Resource reference.
pub struct Ref<'a, T> {
    ptr: NonNull<T>,
    marker: PhantomData<&'a T>,

    #[cfg(feature = "dynamic_wasm")]
    val: MaybeUninit<T>,
}

unsafe impl<T: Send> Send for Ref<'_, T> {}
unsafe impl<T: Sync> Sync for Ref<'_, T> {}

impl<T> Ref<'_, T> {
    /// # Safety
    ///
    /// `ptr` must point to a valid Component or Resource.
    #[inline]
    pub unsafe fn new(ptr: NonNull<T>) -> Self {
        #[cfg(not(feature = "dynamic_wasm"))]
        {
            Self {
                ptr,
                marker: PhantomData,
            }
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            let mut val = MaybeUninit::<T>::uninit();

            if size_of::<T>() != 0 {
                // This is a real type that we need to copy to this wasm
                // instance's memory.
                wasm::wasm_read_external_data(
                    val.as_mut_ptr().cast(),
                    ptr.as_ptr().cast(),
                    size_of::<T>(),
                );
            }

            Self {
                ptr,
                marker: PhantomData,
                val,
            }
        }
    }
}

impl<T> Deref for Ref<'_, T> {
    type Target = T;

    #[cfg(not(feature = "dynamic_wasm"))]
    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }

    #[cfg(feature = "dynamic_wasm")]
    #[inline]
    fn deref(&self) -> &Self::Target {
        if size_of::<T>() == 0 {
            unsafe { self.ptr.as_ref() }
        } else {
            unsafe { self.val.assume_init_ref() }
        }
    }
}

impl<T: Debug> Debug for Ref<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

/// A mutable Component or Resource reference.
pub struct Mut<'a, T> {
    ptr: NonNull<T>,
    marker: PhantomData<&'a mut T>,

    #[cfg(feature = "dynamic_wasm")]
    val: MaybeUninit<T>,
}

unsafe impl<T: Send> Send for Mut<'_, T> {}
unsafe impl<T: Sync> Sync for Mut<'_, T> {}

impl<T> Mut<'_, T> {
    /// # Safety
    ///
    /// `ptr` must point to a valid Component or Resource.
    #[inline]
    pub unsafe fn new(ptr: NonNull<T>) -> Self {
        #[cfg(not(feature = "dynamic_wasm"))]
        {
            Self {
                ptr,
                marker: PhantomData,
            }
        }

        #[cfg(feature = "dynamic_wasm")]
        unsafe {
            let mut val = MaybeUninit::<T>::uninit();

            if size_of::<T>() != 0 {
                // This is a real type that we need to copy to this wasm
                // instance's memory.
                wasm::wasm_read_external_data(
                    val.as_mut_ptr().cast(),
                    ptr.as_ptr().cast(),
                    size_of::<T>(),
                );
            }

            Self {
                ptr,
                marker: PhantomData,
                val,
            }
        }
    }

    /// Returns the pointer to `T` in the ECS storage's memory address space.
    /// With the `dynamic_wasm` feature, this points to an external memory
    /// address space, and the pointer should not be dereferenced.
    pub fn ecs_storage_ptr(&self) -> NonNull<T> {
        self.ptr
    }
}

impl<T> Deref for Mut<'_, T> {
    type Target = T;

    #[cfg(not(feature = "dynamic_wasm"))]
    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }

    #[cfg(feature = "dynamic_wasm")]
    #[inline]
    fn deref(&self) -> &Self::Target {
        if size_of::<T>() == 0 {
            unsafe { self.ptr.as_ref() }
        } else {
            unsafe { self.val.assume_init_ref() }
        }
    }
}

impl<T> DerefMut for Mut<'_, T> {
    #[cfg(not(feature = "dynamic_wasm"))]
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }

    #[cfg(feature = "dynamic_wasm")]
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        if size_of::<T>() == 0 {
            unsafe { self.ptr.as_mut() }
        } else {
            unsafe { self.val.assume_init_mut() }
        }
    }
}

impl<T: Debug> Debug for Mut<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

#[cfg(feature = "dynamic_wasm")]
impl<T> Drop for Mut<'_, T> {
    fn drop(&mut self) {
        if size_of::<T>() == 0 {
            return;
        }

        unsafe {
            // Write changes back to engine memory.
            wasm::wasm_write_external_data(
                self.ptr.as_ptr().cast(),
                (&self.val as *const MaybeUninit<T>).cast(),
                size_of::<T>(),
            );
        }
    }
}

// Global callback functions. When adding a function here, make sure to also
// update the codegen crate responsible for generating the FFI boilerplate code,
// as well as the `get_core_proc_addr` function in the `c_api` mod.

pub static mut _LOAD_SCENE: Option<unsafe extern "C" fn(scene_str: *const c_char)> = None;

// spawning
pub static mut _SPAWN: Option<
    unsafe extern "C" fn(*const ComponentRef<'_>, usize) -> Option<EntityId>,
> = None;

pub static mut _DESPAWN: Option<unsafe extern "C" fn(EntityId)> = None;

pub static mut _ADD_COMPONENTS_FN: Option<
    unsafe extern "C" fn(EntityId, *const ComponentRef<'_>, usize),
> = None;

pub static mut _REMOVE_COMPONENTS_FN: Option<
    unsafe extern "C" fn(EntityId, *const ComponentId, usize),
> = None;

pub static mut _ENTITY_LABEL_FN: Option<unsafe extern "C" fn(EntityId) -> *const c_char> = None;

pub static mut _SET_ENTITY_LABEL_FN: Option<unsafe extern "C" fn(EntityId, *const c_char)> = None;

// events
pub static mut _EVENT_COUNT_FN: Option<unsafe extern "C" fn(*const c_void) -> usize> = None;

pub static mut _EVENT_GET_FN: Option<
    unsafe extern "C" fn(*const c_void, usize) -> Option<NonNull<u64>>,
> = None;

pub static mut _EVENT_SEND_FN: Option<unsafe extern "C" fn(*const c_void, *const u8, usize)> = None;

pub static mut _CALL_FN: Option<unsafe extern "C" fn(ComponentId, *const c_void, usize)> = None;

pub static mut _CALL_ASYNC_FN: Option<
    unsafe extern "C" fn(ComponentId, *const c_void, usize, *const c_void, usize),
> = None;

// parentage
pub static mut _SET_PARENT_FN: Option<unsafe extern "C" fn(EntityId, Option<EntityId>, bool)> =
    None;

pub static mut _GET_PARENT_FN: Option<
    unsafe extern "C" fn(EntityId, *mut MaybeUninit<Option<EntityId>>) -> bool,
> = None;

// system meta
pub static mut _SET_SYSTEM_ENABLED_FN: Option<unsafe extern "C" fn(*const c_char, bool)> = None;

#[cfg(feature = "dynamic_wasm")]
pub mod wasm {
    use std::mem::offset_of;

    use super::*;

    unsafe extern "C" {
        pub fn wasm_write_external_data(dst: *mut c_void, src: *const c_void, bytes: usize);
        pub fn wasm_read_external_data(dst: *mut c_void, src: *const c_void, bytes: usize);
        pub fn wasm_read_external_cstr_len(ptr: *const c_char) -> usize;
        pub fn wasm_alloc_external_data(size: usize, align: usize) -> *mut c_void;
        pub fn wasm_dealloc_external_data(ptr: *mut c_void, size: usize, align: usize);
    }

    /// Helper function used to call external functions which take a `&mut T`.
    /// This will write the current value of `Mut<T>` (which may have been
    /// modified by the Wasm instance) to external memory, calls `f` with the
    /// external pointer value, then reads the value of `T` from external memory
    /// back to Wasm instance memory (which may have been modified during the
    /// function call).
    pub fn update_external_mut<T, U, F>(val: &mut Mut<'_, T>, f: F) -> U
    where
        F: FnOnce(NonNull<T>) -> U,
    {
        let size = size_of::<T>();

        unsafe {
            wasm_write_external_data(val.ptr.as_ptr().cast(), val.val.as_mut_ptr().cast(), size);
            let res = f(val.ptr);
            wasm_read_external_data(val.val.as_mut_ptr().cast(), val.ptr.as_ptr().cast(), size);
            res
        }
    }

    /// Helper function which allocates some external memory, writes `val` to
    /// it, calls `f` with a pointer to the external memory allocation, and then
    /// deallocates the external memory.
    pub fn alloc_and_write_external<T, U, F>(val: &T, f: F) -> U
    where
        F: FnOnce(*const T) -> U,
    {
        let alloc_size = size_of::<T>();
        let alloc_align = align_of::<T>();

        unsafe {
            let ptr = wasm_alloc_external_data(alloc_size, alloc_align);
            wasm_write_external_data(ptr, (val as *const T).cast(), alloc_size);
            let res = f(ptr.cast());
            wasm_dealloc_external_data(ptr, alloc_size, alloc_align);
            res
        }
    }

    /// Helper function which allocates some external memory, writes `data` to
    /// it, calls `f` with a pointer to the external memory allocation, and then
    /// deallocates the external memory.
    pub fn alloc_and_write_external_slice<T, U, F>(data: &[T], f: F) -> U
    where
        T: Copy + 'static,
        F: FnOnce(*const T) -> U,
    {
        let alloc_size = size_of_val(data);
        let alloc_align = align_of::<T>();

        unsafe {
            let ptr = wasm_alloc_external_data(alloc_size, alloc_align);
            wasm_write_external_data(ptr, data.as_ptr().cast(), alloc_size);
            let res = f(ptr.cast());
            wasm_dealloc_external_data(ptr, alloc_size, alloc_align);
            res
        }
    }

    /// Helper function which allocates some uninitialized external memory with
    /// size and alignment `T`, calls `f` with a pointer to the external memory
    /// allocation, reads it back to `data`, and then deallocates the external
    /// memory.
    pub fn alloc_uninit_and_read_external<T, U, F>(data: &mut MaybeUninit<T>, f: F) -> U
    where
        F: FnOnce(*mut MaybeUninit<T>) -> U,
    {
        let alloc_size = size_of_val(data);
        let alloc_align = align_of_val(data);

        unsafe {
            let ptr = wasm_alloc_external_data(alloc_size, alloc_align);
            let res = f(ptr.cast());
            wasm_read_external_data((data as *mut MaybeUninit<T>).cast(), ptr, alloc_size);
            wasm_dealloc_external_data(ptr, alloc_size, alloc_align);
            res
        }
    }

    /// Helper function which allocates some uninitialized external memory large
    /// enough to hold `data` calls `f` with a pointer to `data` in the external
    /// memory allocation, reads the external `data` allocation back to local
    /// `data`, and then deallocates the external memory.
    pub fn alloc_and_read_external<D, U, F>(data: &mut MaybeUninit<D>, f: F) -> U
    where
        D: Copy + 'static,
        F: FnOnce(*mut MaybeUninit<D>) -> U,
    {
        let alloc_size = size_of::<MaybeUninit<D>>();
        let alloc_align = align_of::<MaybeUninit<D>>();

        unsafe {
            let ptr = wasm_alloc_external_data(alloc_size, alloc_align);
            let res = f(ptr.cast());
            wasm_read_external_data(data.as_mut_ptr().cast(), ptr, alloc_size);
            wasm_dealloc_external_data(ptr, alloc_size, alloc_align);
            res
        }
    }

    /// Helper function which allocates some uninitialized external memory large
    /// enough to hold both `val` and `data`. It copies `val` to the external
    /// allocation, calls `f` with a pointer to both `val` and `data` in the
    /// external memory allocation, reads the external `data` allocation back to
    /// local `data`, and then deallocates the external memory.
    pub fn alloc_write_and_read_external<T, D, U, F>(val: T, data: &mut MaybeUninit<D>, f: F) -> U
    where
        T: Copy + 'static,
        D: Copy + 'static,
        F: FnOnce(*mut T, *mut MaybeUninit<D>) -> U,
    {
        struct Payload<T, D> {
            val: T,
            data: MaybeUninit<D>,
        }

        let payload = Payload {
            val,
            data: MaybeUninit::<D>::uninit(),
        };

        let alloc_size = size_of_val(&payload);
        let alloc_align = align_of_val(&payload);

        unsafe {
            let ptr = wasm_alloc_external_data(alloc_size, alloc_align);
            let val_ptr = ptr.add(offset_of!(Payload<T, D>, val));
            let data_ptr = ptr.add(offset_of!(Payload<T, D>, data));
            wasm_write_external_data(val_ptr, (&payload.val as *const T).cast(), size_of::<T>());
            let res = f(val_ptr.cast(), data_ptr.cast());
            wasm_read_external_data(data.as_mut_ptr().cast(), data_ptr, alloc_size);
            wasm_dealloc_external_data(ptr, alloc_size, alloc_align);
            res
        }
    }

    /// Helper function which allocates some uninitialized external memory, deep
    /// copies `components` and its referenced component data to the external
    /// allocation, calls `f` with the `ComponentRef` slice in the external
    /// memory allocation, and then deallocates the external memory.
    pub fn alloc_components_external<F, U>(components: &[ComponentRef<'_>], f: F) -> U
    where
        F: FnOnce(&[ComponentRef<'_>]) -> U,
    {
        // Determine size of full allocation.
        let component_refs_size = size_of_val(components);
        let component_data_size = components
            .iter()
            .map(|component| component.component_size)
            .sum::<usize>();

        let alloc_size = component_refs_size + component_data_size;
        let alloc_align = align_of::<ComponentRef>();

        unsafe {
            // Make allocation.
            let ptr = wasm_alloc_external_data(alloc_size, alloc_align);

            // Write component refs (with external ptr) and component data.
            let mut component_refs_offset = 0;
            let mut component_data_offset = component_refs_size;
            for component in components {
                let refs = ComponentRef {
                    component_id: component.component_id,
                    component_size: component.component_size,
                    component_val: ptr.add(component_data_offset),
                    marker: PhantomData,
                };

                wasm_write_external_data(
                    ptr.add(component_refs_offset),
                    (&refs as *const ComponentRef).cast(),
                    size_of::<ComponentRef>(),
                );

                wasm_write_external_data(
                    ptr.add(component_data_offset),
                    component.component_val,
                    component.component_size,
                );

                component_refs_offset += size_of::<ComponentRef>();
                component_data_offset += component.component_size;
            }

            let res = f(slice::from_raw_parts(
                ptr.cast::<ComponentRef>(),
                components.len(),
            ));

            wasm_dealloc_external_data(ptr, alloc_size, alloc_align);

            res
        }
    }
}

pub mod ffi {
    use super::*;
    include!(env!("ECS_GENERATED_PATH"));
}

// ENGINE INTERNALS - NOT COPIED TO RELEASE HEADERS

#[repr(u8)]
#[derive(Debug)]
pub enum ComponentType {
    AsyncCompletion,
    Component,
    Resource,
}

impl ComponentType {
    pub fn from_u8(val: u8) -> Option<Self> {
        let variant = match val {
            0 => Self::AsyncCompletion,
            1 => Self::Component,
            2 => Self::Resource,
            _ => {
                return None;
            }
        };

        Some(variant)
    }
}

#[repr(u8)]
#[derive(Debug)]
pub enum ArgType {
    Completion,
    DataAccessMut,
    DataAccessRef,
    EventReader,
    EventWriter,
    Query,
}

impl ArgType {
    pub fn from_u8(val: u8) -> Option<Self> {
        let variant = match val {
            0 => Self::Completion,
            1 => Self::DataAccessMut,
            2 => Self::DataAccessRef,
            3 => Self::EventReader,
            4 => Self::EventWriter,
            5 => Self::Query,
            _ => {
                return None;
            }
        };

        Some(variant)
    }
}

/// `FfiVec` is intended to be used when transferring a Rust side Vec to C via
/// FFI. Because [`Vec::into_raw_parts`] isn't stable, we manually create our
/// own in the form of this struct. This memory should not be freed on the C
/// side, but instead should be passed into a Rust provided free function which
/// will call [`Vec::from_raw_parts`] with the parameters in this struct and
/// then free the memory.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct FfiVec<T> {
    pub ptr: *mut T,
    pub len: usize,
    pub capacity: usize,
}
