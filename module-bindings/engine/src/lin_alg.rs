use glam::{Mat3, Mat4, Vec2, Vec3};

use crate::{Transform, event};

impl From<&event::Vec2> for Vec2 {
    fn from(value: &event::Vec2) -> Self {
        Self::new(value.x(), value.y())
    }
}

impl From<Vec2> for event::Vec2 {
    fn from(value: Vec2) -> Self {
        Self::new(value.x, value.y)
    }
}

impl From<&event::Vec3> for Vec3 {
    fn from(value: &event::Vec3) -> Self {
        Self::new(value.x(), value.y(), value.z())
    }
}

impl From<Vec3> for event::Vec3 {
    fn from(value: Vec3) -> Self {
        Self::new(value.x, value.y, value.z)
    }
}

impl From<&event::Mat3x3> for Mat3 {
    fn from(value: &event::Mat3x3) -> Self {
        Self::from_cols_array(&value.unpack().m)
    }
}

impl From<&event::Transform> for Transform {
    fn from(value: &event::Transform) -> Self {
        Self {
            position: Vec2::new(value.position().x(), value.position().y()),
            layer: value.position().z(),
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
            &event::Vec3::new(value.position.x, value.position.y, value.layer),
            &(value.scale).into(),
            &(value.skew).into(),
            &(value.pivot).into(),
            value.rotation,
        )
    }
}

/// Creates a lh orthographic matrix with a depth range of `[0,1]`
#[rustfmt::skip]
pub fn create_orthographic_matrix(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) -> Mat4 {
    let recip_width = 1.0 / (right - left);
    let recip_height = 1.0 / (top - bottom);
    let recip_depth = 1.0 / (far - near);
    Mat4::from_cols_array(&[
            2.0 * recip_width,               0.0,                              0.0,                      0.0,
            0.0,                             2.0 * recip_height,               0.0,                      0.0,
            0.0,                             0.0,                              recip_depth,              0.0,
            -((right + left) * recip_width), -((top + bottom) * recip_height), -(near * recip_depth),    1.0,],
        )
}
