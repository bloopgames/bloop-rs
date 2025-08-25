use glam::{Mat4, Quat, Vec2, Vec3, Vec4, Vec4Swizzles};

use crate::{Camera, LocalToWorld, Transform};

/// Convert the given world-space `position` into the local coordinate system of the given `camera`.
pub fn world_to_view(position: &Vec3, camera: &Camera) -> Vec3 {
    (*camera.__view_matrix * position.extend(1f32)).truncate()
}

/// Convert the given world-space `position` into homogenous clip space of the given `camera`.
pub fn world_to_clip(position: &Vec3, camera: &Camera) -> Vec3 {
    let world_to_clip = get_world_to_clip_matrix(camera);
    (world_to_clip * position.extend(1f32)).truncate()
}

/// Convert the given world-space `position` into the screen space of the given `camera` at the given
/// `screen_dimensions` in pixels.
pub fn world_to_screen(position: &Vec3, screen_dimensions: &Vec2, camera: &Camera) -> Vec2 {
    let world_position = position.extend(1f32);
    let world_to_clip = get_world_to_clip_matrix(camera);

    let clip_to_screen = get_clip_to_screen_matrix(screen_dimensions.x, screen_dimensions.y);

    let world_to_screen = clip_to_screen * world_to_clip;

    (world_to_screen * world_position).xy()
}

/// Convert the given screen-space `screen_position` using the given `camera` and `screen_dimensions`
/// into a world-space position.
pub fn screen_to_world(screen_position: &Vec2, screen_dimensions: &Vec2, camera: &Camera) -> Vec3 {
    let screen_pos = Vec4::new(screen_position.x, screen_position.y, 0.0, 1.0);
    let screen_to_view =
        get_screen_to_view_matrix(camera, screen_dimensions.x, screen_dimensions.y);
    let screen_to_world = camera.__view_matrix.inverse() * screen_to_view;

    // convert into world coordinates
    let world_coordinates = screen_to_world * screen_pos;

    world_coordinates.truncate()
}

/// Convert the given screen-space `screen_position` using the given `screen_dimensions` into its
/// homogenous clip space equivalent.
pub fn screen_to_clip(screen_position: &Vec2, screen_dimensions: &Vec2) -> Vec3 {
    let screen_position = Vec4::new(screen_position.x, screen_position.y, 0.0, 1.0);
    let clip_coordinates =
        get_screen_to_clip_matrix(screen_dimensions.x, screen_dimensions.y) * screen_position;

    clip_coordinates.truncate()
}

/// Convert the given screen-space `screen_position` using the `screen_dimensions`
/// into the local coordinate system of `camera`.
pub fn screen_to_view(screen_position: &Vec2, screen_dimensions: &Vec2, camera: &Camera) -> Vec3 {
    let screen_position = Vec4::new(screen_position.x, screen_position.y, 0.0, 1.0);
    let view_coordinates =
        get_screen_to_view_matrix(camera, screen_dimensions.x, screen_dimensions.y)
            * screen_position;

    view_coordinates.truncate()
}

/// Convert the given `world-position` into the local coordinate system represented by `local_to_world`
pub fn world_to_local(world_position: &Vec2, local_to_world: &LocalToWorld) -> Vec2 {
    local_to_world
        .model_matrix()
        .inverse()
        .mul_vec4(Vec4::new(world_position.x, world_position.y, 0.0, 1.0))
        .xy()
}

/// Convert the given `local_position` in the coordinate system of `local_to_world` into world-space
#[inline]
pub fn local_to_world(local_position: &Vec2, local_to_world: &LocalToWorld) -> Vec2 {
    local_to_world
        .model_matrix()
        .mul_vec4(Vec4::new(local_position.x, local_position.y, 0.0, 1.0))
        .xy()
}

/// Returns the world-space position of the entity associated with the given `LocalToWorld` component
#[inline]
pub fn world_position(local_to_world: &LocalToWorld) -> Vec2 {
    local_to_world.model_matrix().w_axis.xy()
}

/// Set the position of the given `transform` to world-space relative position using the associated
/// `local_to_world` matrix
pub fn set_world_position(
    transform: &mut Transform,
    local_to_world: &LocalToWorld,
    world_position: &Vec2,
) {
    let local_to_parent = Mat4::from_scale_rotation_translation(
        transform.scale.extend(0f32),
        Quat::from_rotation_z(transform.rotation),
        *transform.position,
    );

    let world_to_parent = local_to_parent * local_to_world.model_matrix().inverse();

    let new_local_pos = world_to_parent * Vec4::new(world_position.x, world_position.y, 0.0, 1.0);
    transform.position.x = new_local_pos.x;
    transform.position.y = new_local_pos.y;
}

fn get_screen_to_view_matrix(cam: &Camera, screen_width: f32, screen_height: f32) -> Mat4 {
    cam.__projection_matrix.inverse() * get_screen_to_clip_matrix(screen_width, screen_height)
}

fn get_screen_to_clip_matrix(width: f32, height: f32) -> Mat4 {
    Mat4::from_cols(
        Vec4::new(2.0 / width, 0.0, 0.0, 0.0),
        Vec4::new(0.0, -2.0 / height, 0.0, 0.0),
        Vec4::new(0.0, 0.0, 1.0, 0.0),
        Vec4::new(-1.0, 1.0, 0.0, 1.0),
    )
}

fn get_clip_to_screen_matrix(width: f32, height: f32) -> Mat4 {
    Mat4::from_cols(
        Vec4::new(width * 0.5, 0.0, 0.0, 0.0),
        Vec4::new(0.0, -height * 0.5, 0.0, 0.0),
        Vec4::new(0.0, 0.0, 1.0, 0.0),
        Vec4::new(width * 0.5, height * 0.5, 0.0, 1.0),
    )
}

fn get_world_to_clip_matrix(cam: &Camera) -> Mat4 {
    *(cam.__projection_matrix * cam.__view_matrix)
}
