use std::fmt::Formatter;

use glam::{Mat4, Vec2};
use serde::{
    Deserializer,
    de::{Error, Visitor},
};

use crate::{
    Camera, CameraAspect, FfiOption, Transform,
    colors::Color,
    graphics::{CircleRender, ColorRender, Rect, TextRender},
    text::TextAlignment,
};

pub(crate) fn default_true() -> bool {
    true
}

pub(crate) fn default_color_render_size() -> Vec2 {
    ColorRender::default().size
}

pub(crate) fn default_text_render_font_size() -> f32 {
    TextRender::default().font_size
}

pub(crate) fn default_text_render_alignment() -> TextAlignment {
    TextRender::default().alignment
}

pub(crate) fn default_circle_render_num_sides() -> u32 {
    CircleRender::default().num_sides
}

pub(crate) fn default_transform_scale() -> Vec2 {
    Transform::default().scale
}

pub(crate) fn default_transform_pivot() -> Vec2 {
    Transform::default().pivot
}

pub(crate) fn default_camera_aspect() -> CameraAspect {
    Camera::default().aspect
}

pub(crate) fn default_camera_clear_color() -> Color {
    Camera::default().clear_color
}

pub(crate) fn default_camera_render_order() -> i32 {
    Camera::default().render_order
}

pub(crate) fn default_camera_view_matrix() -> Mat4 {
    Camera::default().__view_matrix
}

pub(crate) fn default_camera_projection_matrix() -> Mat4 {
    Camera::default().__projection_matrix
}

pub(crate) fn default_rect_position() -> Vec2 {
    Rect::default().position
}

pub(crate) fn default_rect_dimensions() -> Vec2 {
    Rect::default().dimensions
}

impl<'de, T: Copy + serde::Deserialize<'de>> serde::Deserialize<'de> for FfiOption<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Option::deserialize(deserializer).map(Self::from)
    }
}

pub(crate) fn deserialize_text_render_text_field<'de, D>(
    deserializer: D,
) -> Result<[u8; crate::graphics::TEXT_RENDER_SIZE], D::Error>
where
    D: Deserializer<'de>,
{
    struct TextRenderTextFieldVisitor;
    impl Visitor<'_> for TextRenderTextFieldVisitor {
        type Value = [u8; crate::graphics::TEXT_RENDER_SIZE];

        fn expecting(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
            formatter.write_str("a sequence of characters with length < 256")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            let str_as_bytes = v.as_bytes();
            let num_bytes = str_as_bytes.len();
            if num_bytes > crate::graphics::TEXT_RENDER_SIZE {
                return Err(Error::invalid_length(num_bytes, &self));
            }

            let mut result: [u8; crate::graphics::TEXT_RENDER_SIZE] =
                [0; crate::graphics::TEXT_RENDER_SIZE];
            result[0..num_bytes].copy_from_slice(&str_as_bytes[0..num_bytes]);
            Ok(result)
        }
    }
    let visitor = TextRenderTextFieldVisitor {};
    deserializer.deserialize_str(visitor)
}
