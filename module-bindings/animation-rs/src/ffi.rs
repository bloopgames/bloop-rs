#![allow(clippy::type_complexity)]

use std::{
    ffi::c_void,
    mem::{MaybeUninit, transmute},
    ptr::NonNull,
};

use engine::{FfiStr, dst::vec::BackingStore, prelude::*};

use super::*;

static mut SPRITE_ANIMATIONS_CID: Option<EcsTypeId> = None;

impl EcsType for SpriteAnimations {
    fn id() -> EcsTypeId {
        unsafe { SPRITE_ANIMATIONS_CID.expect("ComponentId unassigned") }
    }

    unsafe fn set_id(id: EcsTypeId) {
        unsafe {
            SPRITE_ANIMATIONS_CID = Some(id);
        }
    }

    fn string_id() -> &'static str {
        "animation::SpriteAnimations"
    }

    fn null_terminated_string_id() -> &'static str {
        concat!("animation::SpriteAnimations", '\0')
    }
}

pub static mut SPRITE_ANIMATIONS_LOAD_ASEPRITE_ANIMATION: Option<
    unsafe extern "C" fn(
        sprite_animations: *mut SpriteAnimations,
        path_bytes: *const u8,
        path_len: usize,
    ) -> SpriteAnimationId,
> = None;

pub static mut SPRITE_ANIMATIONS_LOAD_CUSTOM_ANIMATION: Option<
    unsafe extern "C" fn(
        sprite_animations: *mut SpriteAnimations,
        frames_backing_store: Option<BackingStore<SpriteAnimationFrame>>,
        frames_len: u32,
        tags_backing_store: Option<BackingStore<SpriteAnimationTag>>,
        tags_len: u32,
    ) -> SpriteAnimationId,
> = None;

pub static mut SPRITE_ANIMATIONS_GET: Option<
    unsafe extern "C" fn(
        sprite_animations: *const SpriteAnimations,
        id: *const SpriteAnimationId,
    ) -> Option<NonNull<SpriteAnimationAssetState>>,
> = None;

pub static mut SPRITE_ANIMATIONS_SET_ANIMATION_TAG: Option<
    unsafe extern "C" fn(
        sprite_animations: *const SpriteAnimations,
        animation: *mut SpriteAnimation,
        tag: *const FfiStr<'_>,
    ) -> Option<NonZero<u32>>,
> = None;

static mut SPRITE_ANIMATOR_CID: Option<EcsTypeId> = None;

impl EcsType for SpriteAnimation {
    fn id() -> EcsTypeId {
        unsafe { SPRITE_ANIMATOR_CID.expect("ComponentId unassigned") }
    }

    unsafe fn set_id(id: EcsTypeId) {
        unsafe {
            SPRITE_ANIMATOR_CID = Some(id);
        }
    }

    fn string_id() -> &'static str {
        "animation::SpriteAnimation"
    }

    fn null_terminated_string_id() -> &'static str {
        concat!("animation::SpriteAnimation", '\0')
    }
}

impl Component for SpriteAnimation {}

pub static mut SPRITE_ANIMATION_NEW: Option<
    unsafe extern "C" fn(
        sprite_animation_id: SpriteAnimationId,
        ret: *mut MaybeUninit<SpriteAnimation>,
    ),
> = None;

pub static mut SPRITE_ANIMATION_PLAY: Option<
    unsafe extern "C" fn(sprite_animation: NonNull<SpriteAnimation>),
> = None;

pub static mut SPRITE_ANIMATION_PAUSE: Option<
    unsafe extern "C" fn(sprite_animation: NonNull<SpriteAnimation>),
> = None;

pub static mut SPRITE_ANIMATION_SET_ACTIVE_ANIMATION: Option<
    unsafe extern "C" fn(
        sprite_animation: NonNull<SpriteAnimation>,
        animation_id: SpriteAnimationId,
    ),
> = None;

#[allow(clippy::missing_transmute_annotations, clippy::missing_safety_doc)]
pub unsafe fn load_module_proc_addrs(
    get_proc_addr: unsafe extern "C" fn(*const u8, *mut c_void) -> Option<NonNull<c_void>>,
    ctx: *mut c_void,
) {
    unsafe {
        SPRITE_ANIMATIONS_LOAD_ASEPRITE_ANIMATION = transmute(get_proc_addr(
            c"animation::SpriteAnimations::load_aseprite_animation"
                .as_ptr()
                .cast(),
            ctx,
        ));
        SPRITE_ANIMATIONS_LOAD_CUSTOM_ANIMATION = transmute(get_proc_addr(
            c"animation::SpriteAnimations::load_custom_animation"
                .as_ptr()
                .cast(),
            ctx,
        ));
        SPRITE_ANIMATIONS_GET = transmute(get_proc_addr(
            c"animation::SpriteAnimations::get".as_ptr().cast(),
            ctx,
        ));
        SPRITE_ANIMATIONS_SET_ANIMATION_TAG = transmute(get_proc_addr(
            c"animation::SpriteAnimations::set_animation_tag"
                .as_ptr()
                .cast(),
            ctx,
        ));
        SPRITE_ANIMATION_NEW = transmute(get_proc_addr(
            c"animation::SpriteAnimation::new".as_ptr().cast(),
            ctx,
        ));
        SPRITE_ANIMATION_PLAY = transmute(get_proc_addr(
            c"animation::SpriteAnimation::play".as_ptr().cast(),
            ctx,
        ));
        SPRITE_ANIMATION_PAUSE = transmute(get_proc_addr(
            c"animation::SpriteAnimation::pause".as_ptr().cast(),
            ctx,
        ));
        SPRITE_ANIMATION_SET_ACTIVE_ANIMATION = transmute(get_proc_addr(
            c"animation::SpriteAnimation::set_active_animation"
                .as_ptr()
                .cast(),
            ctx,
        ));
    }
}

#[allow(clippy::missing_safety_doc)]
pub unsafe fn set_ecs_type_id(string_id: &str, id: EcsTypeId) {
    if string_id == SpriteAnimations::string_id() {
        unsafe {
            SpriteAnimations::set_id(id);
        }
    } else if string_id == SpriteAnimation::string_id() {
        unsafe {
            SpriteAnimation::set_id(id);
        }
    }
}
