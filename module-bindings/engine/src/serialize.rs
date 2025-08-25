use std::fmt::Formatter;

use serde::{
    Deserialize, Deserializer,
    de::{Error, MapAccess, SeqAccess, Visitor},
};

use crate::{
    Camera, CameraAspect, FfiOption, Transform,
    colors::Color,
    graphics::{CircleRender, ColorRender, Rect, TextRender, TextureId},
    linalg::{Mat4, Vec2, Vec3, Vec4},
    material::{MaterialId, MaterialParameters, TEXTURE_LIMIT, UNIFORM_LIMIT},
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

pub(crate) fn default_camera_render_target_texture_id() -> FfiOption<u32> {
    Camera::default().render_target_texture_id
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

pub(crate) fn default_material_parameters_material_id() -> MaterialId {
    MaterialParameters::default().material_id()
}

pub(crate) fn default_material_parameters_textures() -> [Option<TextureId>; TEXTURE_LIMIT] {
    MaterialParameters::default().textures
}

pub(crate) fn default_material_parameters_data() -> [f32; UNIFORM_LIMIT] {
    MaterialParameters::default().data
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

impl<'de> serde::Deserialize<'de> for Vec2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Vec2Visitor;

        #[derive(serde::Deserialize)]
        #[allow(non_camel_case_types)]
        enum Field {
            x,
            y,
        }

        impl<'de> Visitor<'de> for Vec2Visitor {
            type Value = Vec2;

            fn expecting(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("`x` or `y`")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let x = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(0, &self))?;
                let y = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(1, &self))?;
                Ok(Self::Value::new(x, y))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut x = None;
                let mut y = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::x => {
                            if x.is_some() {
                                return Err(Error::duplicate_field("x"));
                            }
                            x = Some(map.next_value()?);
                        }
                        Field::y => {
                            if y.is_some() {
                                return Err(Error::duplicate_field("y"));
                            }
                            y = Some(map.next_value()?);
                        }
                    }
                }

                let x = x.ok_or_else(|| Error::missing_field("x"))?;
                let y = y.ok_or_else(|| Error::missing_field("y"))?;
                Ok(Self::Value::new(x, y))
            }
        }

        const FIELDS: &[&str] = &["x", "y"];
        deserializer.deserialize_struct("Vec2", FIELDS, Vec2Visitor)
    }
}

impl<'de> Deserialize<'de> for Vec3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Vec3Visitor;

        #[derive(serde::Deserialize)]
        #[allow(non_camel_case_types)]
        enum Field {
            x,
            y,
            z,
        }

        impl<'de> Visitor<'de> for Vec3Visitor {
            type Value = Vec3;

            fn expecting(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("`x`, `y`, or `z`")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let x = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(0, &self))?;
                let y = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(1, &self))?;
                let z = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(2, &self))?;
                Ok(Self::Value::new(x, y, z))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut x = None;
                let mut y = None;
                let mut z = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::x => {
                            if x.is_some() {
                                return Err(Error::duplicate_field("x"));
                            }
                            x = Some(map.next_value()?);
                        }
                        Field::y => {
                            if y.is_some() {
                                return Err(Error::duplicate_field("y"));
                            }
                            y = Some(map.next_value()?);
                        }
                        Field::z => {
                            if z.is_some() {
                                return Err(Error::duplicate_field("z"));
                            }
                            z = Some(map.next_value()?);
                        }
                    }
                }

                let x = x.ok_or_else(|| Error::missing_field("x"))?;
                let y = y.ok_or_else(|| Error::missing_field("y"))?;
                let z = z.ok_or_else(|| Error::missing_field("z"))?;
                Ok(Self::Value::new(x, y, z))
            }
        }

        const FIELDS: &[&str] = &["x", "y", "z"];
        deserializer.deserialize_struct("Vec3", FIELDS, Vec3Visitor)
    }
}

impl<'de> Deserialize<'de> for Vec4 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Vec4Visitor;

        #[derive(serde::Deserialize)]
        #[allow(non_camel_case_types)]
        enum Field {
            x,
            y,
            z,
            w,
        }

        impl<'de> Visitor<'de> for Vec4Visitor {
            type Value = Vec4;

            fn expecting(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("`x`, `y`, `z`, or `w`")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let x = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(0, &self))?;
                let y = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(1, &self))?;
                let z = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(2, &self))?;
                let w = seq
                    .next_element()?
                    .ok_or_else(|| Error::invalid_length(3, &self))?;
                Ok(Self::Value::new(x, y, z, w))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut x = None;
                let mut y = None;
                let mut z = None;
                let mut w = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::x => {
                            if x.is_some() {
                                return Err(Error::duplicate_field("x"));
                            }
                            x = Some(map.next_value()?);
                        }
                        Field::y => {
                            if y.is_some() {
                                return Err(Error::duplicate_field("y"));
                            }
                            y = Some(map.next_value()?);
                        }
                        Field::z => {
                            if z.is_some() {
                                return Err(Error::duplicate_field("z"));
                            }
                            z = Some(map.next_value()?);
                        }
                        Field::w => {
                            if w.is_some() {
                                return Err(Error::duplicate_field("w"));
                            }
                            w = Some(map.next_value()?);
                        }
                    }
                }

                let x = x.ok_or_else(|| Error::missing_field("x"))?;
                let y = y.ok_or_else(|| Error::missing_field("y"))?;
                let z = z.ok_or_else(|| Error::missing_field("z"))?;
                let w = w.ok_or_else(|| Error::missing_field("w"))?;
                Ok(Self::Value::new(x, y, z, w))
            }
        }

        const FIELDS: &[&str] = &["x", "y", "z", "w"];
        deserializer.deserialize_struct("Vec4", FIELDS, Vec4Visitor)
    }
}

impl<'de> Deserialize<'de> for Mat4 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Mat4Visitor;

        #[derive(serde::Deserialize)]
        #[allow(non_camel_case_types)]
        enum Field {
            x_axis,
            y_axis,
            z_axis,
            w_axis,
        }

        impl<'de> Visitor<'de> for Mat4Visitor {
            type Value = Mat4;

            fn expecting(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
                formatter.write_str(concat!(
                    "a sequence of 16 f32 values or an object containing 4 Vec4 fields"
                ))
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Mat4, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let mut f = { [0.0; 16] };
                for (i, item) in f.iter_mut().enumerate() {
                    *item = seq
                        .next_element()?
                        .ok_or_else(|| Error::invalid_length(i, &self))?;
                }
                Ok(Mat4::new(glam::Mat4::from_cols_array(&f)))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                // deserialize these as our Vec4 wrapper so that they use the map access defined for Vec4
                // above
                let mut x_axis: Option<Vec4> = None;
                let mut y_axis: Option<Vec4> = None;
                let mut z_axis: Option<Vec4> = None;
                let mut w_axis: Option<Vec4> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::x_axis => {
                            if x_axis.is_some() {
                                return Err(Error::duplicate_field("x_axis"));
                            }
                            x_axis = Some(map.next_value()?);
                        }
                        Field::y_axis => {
                            if y_axis.is_some() {
                                return Err(Error::duplicate_field("y_axis"));
                            }
                            y_axis = Some(map.next_value()?);
                        }
                        Field::z_axis => {
                            if z_axis.is_some() {
                                return Err(Error::duplicate_field("z_axis"));
                            }
                            z_axis = Some(map.next_value()?);
                        }
                        Field::w_axis => {
                            if w_axis.is_some() {
                                return Err(Error::duplicate_field("w_axis"));
                            }
                            w_axis = Some(map.next_value()?);
                        }
                    }
                }

                let x_axis = x_axis.ok_or_else(|| Error::missing_field("x_axis"))?;
                let y_axis = y_axis.ok_or_else(|| Error::missing_field("y_axis"))?;
                let z_axis = z_axis.ok_or_else(|| Error::missing_field("z_axis"))?;
                let w_axis = w_axis.ok_or_else(|| Error::missing_field("w_axis"))?;
                Ok(Mat4::new(glam::Mat4::from_cols(
                    *x_axis, *y_axis, *z_axis, *w_axis,
                )))
            }
        }

        const FIELDS: &[&str] = &["x_axis", "y_axis", "z_axis", "w_axis"];
        deserializer.deserialize_struct("Mat4", FIELDS, Mat4Visitor)
    }
}
