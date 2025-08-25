use snapshot_derive::{DeserializeEngine, SerializeEngine};

use crate::{Transform, event};

/// An explicit wrapper struct around the `glam::Vec2` struct. This wrapper exist in order to have direct
/// control over the serde implementation for it.
///
/// # Example
///
/// ```
/// use engine::linalg::Vec2;
///
/// let mut wrapped_vec = Vec2::new(glam::Vec2::new(1.0, 2.0));
/// wrapped_vec += glam::Vec2::splat(10.0);
/// ```
#[repr(C)]
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    PartialEq,
    bytemuck::Pod,
    bytemuck::Zeroable,
    SerializeEngine,
    DeserializeEngine,
    engine_derive::Vector,
)]
pub struct Vec2(glam::Vec2);

impl Vec2 {
    #[inline(always)]
    pub const fn new(x: f32, y: f32) -> Self {
        Self(glam::Vec2::new(x, y))
    }
}

/// An explicit wrapper struct around the `glam::Vec3` struct. This wrapper exist in order to have direct
/// control over the serde implementation for it.
///
/// # Example
///
/// ```
/// use engine::linalg::Vec3;
///
/// let mut wrapped_vec = Vec3::new(glam::Vec3::new(1.0, 2.0, 3.0));
/// wrapped_vec += glam::Vec3::splat(10.0);
/// ```
#[repr(C)]
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    PartialEq,
    bytemuck::Pod,
    bytemuck::Zeroable,
    SerializeEngine,
    DeserializeEngine,
    engine_derive::Vector,
)]
pub struct Vec3(glam::Vec3);

impl Vec3 {
    #[inline(always)]
    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Self(glam::Vec3::new(x, y, z))
    }
}

/// An explicit wrapper struct around the `glam::Vec4` struct. This wrapper exist in order to have direct
/// control over the serde implementation for it.
///
/// # Example
///
/// ```
/// use engine::linalg::Vec4;
///
/// let mut wrapped_vec = Vec4::new(glam::Vec4::new(1.0, 2.0, 3.0, 4.0));
/// wrapped_vec += glam::Vec4::splat(10.0);
/// ```
#[repr(C)]
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    PartialEq,
    bytemuck::Pod,
    bytemuck::Zeroable,
    SerializeEngine,
    DeserializeEngine,
    engine_derive::Vector,
)]
pub struct Vec4(glam::Vec4);

impl Vec4 {
    #[inline(always)]
    pub const fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self(glam::Vec4::new(x, y, z, w))
    }
}

/// An explicit wrapper struct around the `glam::Mat4` struct. This wrapper exist in order to have direct
/// control over the serde implementation for it.
///
/// # Example
///
/// ```
/// use engine::linalg::Mat4;
///
/// let mut wrapped_matrix = Mat4::new(glam::Mat4::from_scale_rotation_translation(
///     glam::Vec3::splat(1.0),
///     glam::Quat::from_rotation_z(45.0_f32.to_radians()),
///     glam::Vec3::new(1.0, 2.0, 3.0)));
///
/// let other_matrix = glam::Mat4::from_scale_rotation_translation(
///     glam::Vec3::splat(1.0),
///     glam::Quat::from_rotation_z(90.0_f32.to_radians()),
///     glam::Vec3::new(10.0, 25.0, -30.0));
/// let result = wrapped_matrix.mul_mat4(&other_matrix);
/// ```
#[repr(C)]
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    PartialEq,
    bytemuck::Pod,
    bytemuck::Zeroable,
    SerializeEngine,
    DeserializeEngine,
    engine_derive::Matrix,
)]
pub struct Mat4(glam::Mat4);

impl std::ops::Mul<Vec4> for Mat4 {
    type Output = Vec4;

    fn mul(self, rhs: Vec4) -> Self::Output {
        (*self * *rhs).into()
    }
}

impl std::ops::Mul<&Vec4> for Mat4 {
    type Output = Vec4;

    fn mul(self, rhs: &Vec4) -> Self::Output {
        self * *rhs
    }
}

impl From<&event::Vec2> for Vec2 {
    fn from(value: &event::Vec2) -> Self {
        Self::new(value.x(), value.y())
    }
}

impl From<&event::Vec2> for glam::Vec2 {
    fn from(value: &event::Vec2) -> Self {
        Self::new(value.x(), value.y())
    }
}

impl From<glam::Vec2> for event::Vec2 {
    fn from(value: glam::Vec2) -> Self {
        Self::new(value.x, value.y)
    }
}

impl From<&event::Mat3x3> for glam::Mat3 {
    fn from(value: &event::Mat3x3) -> Self {
        Self::from_cols_array(&value.unpack().m)
    }
}

impl From<&event::Vec3> for Vec3 {
    fn from(value: &event::Vec3) -> Self {
        Self::new(value.x(), value.y(), value.z())
    }
}

impl From<&event::Vec3> for glam::Vec3 {
    fn from(value: &event::Vec3) -> Self {
        Self::new(value.x(), value.y(), value.z())
    }
}

impl From<glam::Vec3> for event::Vec3 {
    fn from(value: glam::Vec3) -> Self {
        Self::new(value.x, value.y, value.z)
    }
}

impl From<&event::Transform> for Transform {
    fn from(value: &event::Transform) -> Self {
        Self {
            position: value.position().into(),
            rotation: value.rotation(),
            scale: value.scale().into(),
            skew: value.skew().into(),
            pivot: value.pivot().into(),
            _padding: 0.,
        }
    }
}

impl From<Transform> for event::Transform {
    fn from(value: Transform) -> Self {
        Self::new(
            &(*value.position).into(),
            &(*value.scale).into(),
            &(*value.skew).into(),
            &(*value.pivot).into(),
            value.rotation,
        )
    }
}

/// Creates a lh orthographic matrix with a depth range of `[0,1]`
#[rustfmt::skip]
pub fn create_orthographic_matrix(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) -> glam::Mat4 {
    let recip_width = 1.0 / (right - left);
    let recip_height = 1.0 / (top - bottom);
    let recip_depth = 1.0 / (far - near);
    glam::Mat4::from_cols_array(&[
            2.0 * recip_width,                 0.0,                                0.0,                        0.0,
            0.0,                               2.0 * recip_height,                 0.0,                        0.0,
            0.0,                               0.0,                                recip_depth,                0.0,
            -((right + left) * recip_width),   -((top + bottom) * recip_height),   -(near * recip_depth),      1.0,],
        )
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_vec2_add() {
        let a = crate::linalg::Vec2::new(0.0, 0.0);
        let b = crate::linalg::Vec2::new(1.0, 1.0);
        let result = a + b;
        assert_eq!(result.x, 1.0);
        assert_eq!(result.y, 1.0);
    }

    #[test]
    fn test_vec2_add_assign() {
        let mut a = crate::linalg::Vec2::new(0.0, 0.0);
        let b = crate::linalg::Vec2::new(1.0, 1.0);
        a += b;
        assert_eq!(a.x, 1.0);
        assert_eq!(a.y, 1.0);

        let mut a = crate::linalg::Vec2::new(0.0, 0.0);
        let b = glam::Vec2::new(1.0, 1.0);
        a += b;
        assert_eq!(a.x, 1.0);
        assert_eq!(a.y, 1.0);
    }

    #[test]
    fn test_vec2_sub() {
        let a = crate::linalg::Vec2::new(0.0, 0.0);
        let b = crate::linalg::Vec2::new(1.0, 1.0);
        let result = a - b;
        assert_eq!(result.x, -1.0);
        assert_eq!(result.y, -1.0);
    }

    #[test]
    fn test_vec2_sub_assign() {
        let mut a = crate::linalg::Vec2::new(0.0, 0.0);
        let b = crate::linalg::Vec2::new(1.0, 1.0);
        a -= b;
        assert_eq!(a.x, -1.0);
        assert_eq!(a.y, -1.0);

        let mut a = crate::linalg::Vec2::new(0.0, 0.0);
        let b = glam::Vec2::new(1.0, 1.0);
        a -= b;
        assert_eq!(a.x, -1.0);
        assert_eq!(a.y, -1.0);
    }

    #[test]
    fn test_vec2_mul() {
        let a = crate::linalg::Vec2::new(1.0, 1.0);
        let s: f32 = 15.0;
        let result = a * s;
        assert_eq!(result.x, 15.0);
        assert_eq!(result.y, 15.0);
    }

    #[test]
    fn test_vec2_mul_assign() {
        let mut a = crate::linalg::Vec2::new(1.0, 1.0);
        let s: f32 = 15.0;
        a *= s;
        assert_eq!(a.x, 15.0);
        assert_eq!(a.y, 15.0);
    }

    #[test]
    fn test_vec2_div() {
        let a = crate::linalg::Vec2::new(1.0, 1.0);
        let s: f32 = 15.0;
        let result = a / s;
        assert_eq!(result.x, 1.0 / 15.0);
        assert_eq!(result.y, 1.0 / 15.0);
    }

    #[test]
    fn test_vec2_div_assign() {
        let mut a = crate::linalg::Vec2::new(1.0, 1.0);
        let s: f32 = 15.0;
        a /= s;
        assert_eq!(a.x, 1.0 / 15.0);
        assert_eq!(a.y, 1.0 / 15.0);
    }

    #[test]
    fn test_mat4_mul() {
        let a: crate::linalg::Mat4 = glam::Mat4::from_cols_array(&[
            1.0, 2.0, 3.0, 0.0, 4.0, 5.0, 6.0, 0.0, 7.0, 8.0, 9.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ])
        .into();
        let b: crate::linalg::Mat4 = glam::Mat4::from_cols_array(&[
            1.0, 2.0, 3.0, 0.0, 4.0, 5.0, 6.0, 0.0, 7.0, 8.0, 9.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ])
        .into();
        let result = a * b;
        assert_eq!(result.x_axis.x, 30.0);
        assert_eq!(result.x_axis.y, 36.0);
        assert_eq!(result.x_axis.z, 42.0);
        assert_eq!(result.x_axis.w, 0.0);

        assert_eq!(result.y_axis.x, 66.0);
        assert_eq!(result.y_axis.y, 81.0);
        assert_eq!(result.y_axis.z, 96.0);
        assert_eq!(result.y_axis.w, 0.0);

        assert_eq!(result.z_axis.x, 102.0);
        assert_eq!(result.z_axis.y, 126.0);
        assert_eq!(result.z_axis.z, 150.0);
        assert_eq!(result.z_axis.w, 0.0);

        assert_eq!(result.w_axis.x, 0.0);
        assert_eq!(result.w_axis.y, 0.0);
        assert_eq!(result.w_axis.z, 0.0);
        assert_eq!(result.w_axis.w, 1.0);
    }
}
