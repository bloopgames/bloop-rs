use std::{
    fmt,
    num::ParseIntError,
    ops::{Deref, DerefMut},
};

use engine_derive::component;

use crate::{
    Component, ComponentId, EcsType,
    event::{self, graphics::Color as EventColor},
    linalg::Vec4,
};

#[repr(C)]
#[derive(bytemuck::Pod, bytemuck::Zeroable, Debug, PartialEq)]
#[component(engine)]
pub struct Color(Vec4);

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Color({}, {}, {}, {})", self.x, self.y, self.z, self.w)
    }
}

fn try_str_to_num(code: &str) -> Result<f32, HexError> {
    u8::from_str_radix(code, 16)
        .map(|x| f32::from(x) / 255.0)
        .map_err(HexError::ParseError)
}

impl Default for Color {
    fn default() -> Self {
        Self::WHITE
    }
}

impl Color {
    pub const ALICE_BLUE: Color = Color::new(0.941176, 0.972549, 1.0, 1.0);
    pub const ANTIQUE_WHITE: Color = Color::new(0.980392, 0.921569, 0.843137, 1.0);
    pub const AQUA: Color = Color::new(0.0, 1.0, 1.0, 1.0);
    pub const AQUAMARINE: Color = Color::new(0.498039, 1.0, 0.831373, 1.0);
    pub const AZURE: Color = Color::new(0.941176, 1.0, 1.0, 1.0);
    pub const BEIGE: Color = Color::new(0.960784, 0.960784, 0.862745, 1.0);
    pub const BISQUE: Color = Color::new(1.0, 0.894118, 0.768627, 1.0);
    pub const BLACK: Color = Color::new(0.0, 0.0, 0.0, 1.0);
    pub const BLANCHED_ALMOND: Color = Color::new(1.0, 0.921569, 0.803922, 1.0);
    pub const BLUE: Color = Color::new(0.0, 0.0, 1.0, 1.0);
    pub const BLUE_VIOLET: Color = Color::new(0.541176, 0.168627, 0.886275, 1.0);
    pub const BROWN: Color = Color::new(0.647059, 0.164706, 0.164706, 1.0);
    pub const BURLYWOOD: Color = Color::new(0.870588, 0.721569, 0.529412, 1.0);
    pub const CADET_BLUE: Color = Color::new(0.372549, 0.619608, 0.627451, 1.0);
    pub const CHARTREUSE: Color = Color::new(0.498039, 1.0, 0.0, 1.0);
    pub const CHOCOLATE: Color = Color::new(0.823529, 0.411765, 0.117647, 1.0);
    pub const CORAL: Color = Color::new(1.0, 0.498039, 0.313726, 1.0);
    pub const CORNFLOWER_BLUE: Color = Color::new(0.392157, 0.584314, 0.929412, 1.0);
    pub const CORNSILK: Color = Color::new(1.0, 0.972549, 0.862745, 1.0);
    pub const CRIMSON: Color = Color::new(0.862745, 0.0784314, 0.235294, 1.0);
    pub const CYAN: Color = Color::new(0.0, 1.0, 1.0, 1.0);
    pub const DARK_BLUE: Color = Color::new(0.0, 0.0, 0.545098, 1.0);
    pub const DARK_CYAN: Color = Color::new(0.0, 0.545098, 0.545098, 1.0);
    pub const DARK_GOLDENROD: Color = Color::new(0.721569, 0.52549, 0.0431373, 1.0);
    pub const DARK_GRAY: Color = Color::new(0.662745, 0.662745, 0.662745, 1.0);
    pub const DARK_GREEN: Color = Color::new(0.0, 0.392157, 0.0, 1.0);
    pub const DARK_KHAKI: Color = Color::new(0.741176, 0.717647, 0.419608, 1.0);
    pub const DARK_MAGENTA: Color = Color::new(0.545098, 0.0, 0.545098, 1.0);
    pub const DARK_OLIVE_GREEN: Color = Color::new(0.333333, 0.419608, 0.184314, 1.0);
    pub const DARK_ORANGE: Color = Color::new(1.0, 0.54902, 0.0, 1.0);
    pub const DARK_ORCHID: Color = Color::new(0.6, 0.196078, 0.8, 1.0);
    pub const DARK_RED: Color = Color::new(0.545098, 0.0, 0.0, 1.0);
    pub const DARK_SALMON: Color = Color::new(0.913725, 0.588235, 0.478431, 1.0);
    pub const DARK_SEA_GREEN: Color = Color::new(0.560784, 0.737255, 0.560784, 1.0);
    pub const DARK_SLATE_BLUE: Color = Color::new(0.282353, 0.239216, 0.545098, 1.0);
    pub const DARK_SLATE_GRAY: Color = Color::new(0.184314, 0.309804, 0.309804, 1.0);
    pub const DARK_TURQUOISE: Color = Color::new(0.0, 0.807843, 0.819608, 1.0);
    pub const DARK_VIOLET: Color = Color::new(0.580392, 0.0, 0.827451, 1.0);
    pub const DEEP_PINK: Color = Color::new(1.0, 0.0784314, 0.576471, 1.0);
    pub const DEEP_SKY_BLUE: Color = Color::new(0.0, 0.74902, 1.0, 1.0);
    pub const DIM_GRAY: Color = Color::new(0.411765, 0.411765, 0.411765, 1.0);
    pub const DODGER_BLUE: Color = Color::new(0.117647, 0.564706, 1.0, 1.0);
    pub const FIREBRICK: Color = Color::new(0.698039, 0.133333, 0.133333, 1.0);
    pub const FLORAL_WHITE: Color = Color::new(1.0, 0.980392, 0.941176, 1.0);
    pub const FOREST_GREEN: Color = Color::new(0.133333, 0.545098, 0.133333, 1.0);
    pub const FUCHSIA: Color = Color::new(1.0, 0.0, 1.0, 1.0);
    pub const GAINSBORO: Color = Color::new(0.862745, 0.862745, 0.862745, 1.0);
    pub const GHOST_WHITE: Color = Color::new(0.972549, 0.972549, 1.0, 1.0);
    pub const GOLD: Color = Color::new(1.0, 0.843137, 0.0, 1.0);
    pub const GOLDENROD: Color = Color::new(0.854902, 0.647059, 0.12549, 1.0);
    pub const GRAY: Color = Color::new(0.745098, 0.745098, 0.745098, 1.0);
    pub const GREEN: Color = Color::new(0.0, 1.0, 0.0, 1.0);
    pub const GREEN_YELLOW: Color = Color::new(0.678431, 1.0, 0.184314, 1.0);
    pub const HONEYDEW: Color = Color::new(0.941176, 1.0, 0.941176, 1.0);
    pub const HOT_PINK: Color = Color::new(1.0, 0.411765, 0.705882, 1.0);
    pub const INDIGO: Color = Color::new(0.294118, 0.0, 0.509804, 1.0);
    pub const IVORY: Color = Color::new(1.0, 1.0, 0.941176, 1.0);
    pub const KHAKI: Color = Color::new(0.941176, 0.901961, 0.54902, 1.0);
    pub const LAVENDER: Color = Color::new(0.901961, 0.901961, 0.980392, 1.0);
    pub const LAVENDER_BLUSH: Color = Color::new(1.0, 0.941176, 0.960784, 1.0);
    pub const LAWN_GREEN: Color = Color::new(0.486275, 0.988235, 0.0, 1.0);
    pub const LEMON_CHIFFON: Color = Color::new(1.0, 0.980392, 0.803922, 1.0);
    pub const LIGHT_BLUE: Color = Color::new(0.678431, 0.847059, 0.901961, 1.0);
    pub const LIGHT_CORAL: Color = Color::new(0.941176, 0.501961, 0.501961, 1.0);
    pub const LIGHT_CYAN: Color = Color::new(0.878431, 1.0, 1.0, 1.0);
    pub const LIGHT_GOLDENROD: Color = Color::new(0.980392, 0.980392, 0.823529, 1.0);
    pub const LIGHT_GRAY: Color = Color::new(0.827451, 0.827451, 0.827451, 1.0);
    pub const LIGHT_GREEN: Color = Color::new(0.564706, 0.933333, 0.564706, 1.0);
    pub const LIGHT_PINK: Color = Color::new(1.0, 0.713726, 0.756863, 1.0);
    pub const LIGHT_SALMON: Color = Color::new(1.0, 0.627451, 0.478431, 1.0);
    pub const LIGHT_SEA_GREEN: Color = Color::new(0.12549, 0.698039, 0.666667, 1.0);
    pub const LIGHT_SKY_BLUE: Color = Color::new(0.529412, 0.807843, 0.980392, 1.0);
    pub const LIGHT_SLATE_GRAY: Color = Color::new(0.466667, 0.533333, 0.6, 1.0);
    pub const LIGHT_STEEL_BLUE: Color = Color::new(0.690196, 0.768627, 0.870588, 1.0);
    pub const LIGHT_YELLOW: Color = Color::new(1.0, 1.0, 0.878431, 1.0);
    pub const LIME: Color = Color::new(0.0, 1.0, 0.0, 1.0);
    pub const LIME_GREEN: Color = Color::new(0.196078, 0.803922, 0.196078, 1.0);
    pub const LINEN: Color = Color::new(0.980392, 0.941176, 0.901961, 1.0);
    pub const MAGENTA: Color = Color::new(1.0, 0.0, 1.0, 1.0);
    pub const MAROON: Color = Color::new(0.690196, 0.188235, 0.376471, 1.0);
    pub const MEDIUM_AQUAMARINE: Color = Color::new(0.4, 0.803922, 0.666667, 1.0);
    pub const MEDIUM_BLUE: Color = Color::new(0.0, 0.0, 0.803922, 1.0);
    pub const MEDIUM_ORCHID: Color = Color::new(0.729412, 0.333333, 0.827451, 1.0);
    pub const MEDIUM_PURPLE: Color = Color::new(0.576471, 0.439216, 0.858824, 1.0);
    pub const MEDIUM_SEA_GREEN: Color = Color::new(0.235294, 0.701961, 0.443137, 1.0);
    pub const MEDIUM_SLATE_BLUE: Color = Color::new(0.482353, 0.407843, 0.933333, 1.0);
    pub const MEDIUM_SPRING_GREEN: Color = Color::new(0.0, 0.980392, 0.603922, 1.0);
    pub const MEDIUM_TURQUOISE: Color = Color::new(0.282353, 0.819608, 0.8, 1.0);
    pub const MEDIUM_VIOLET_RED: Color = Color::new(0.780392, 0.0823529, 0.521569, 1.0);
    pub const MIDNIGHT_BLUE: Color = Color::new(0.0980392, 0.0980392, 0.439216, 1.0);
    pub const MINT_CREAM: Color = Color::new(0.960784, 1.0, 0.980392, 1.0);
    pub const MISTY_ROSE: Color = Color::new(1.0, 0.894118, 0.882353, 1.0);
    pub const MOCCASIN: Color = Color::new(1.0, 0.894118, 0.709804, 1.0);
    pub const NAVAJO_WHITE: Color = Color::new(1.0, 0.870588, 0.678431, 1.0);
    pub const NAVY_BLUE: Color = Color::new(0.0, 0.0, 0.501961, 1.0);
    pub const OLD_LACE: Color = Color::new(0.992157, 0.960784, 0.901961, 1.0);
    pub const OLIVE: Color = Color::new(0.501961, 0.501961, 0.0, 1.0);
    pub const OLIVE_DRAB: Color = Color::new(0.419608, 0.556863, 0.137255, 1.0);
    pub const ORANGE: Color = Color::new(1.0, 0.647059, 0.0, 1.0);
    pub const ORANGE_RED: Color = Color::new(1.0, 0.270588, 0.0, 1.0);
    pub const ORCHID: Color = Color::new(0.854902, 0.439216, 0.839216, 1.0);
    pub const PALE_GOLDENROD: Color = Color::new(0.933333, 0.909804, 0.666667, 1.0);
    pub const PALE_GREEN: Color = Color::new(0.596078, 0.984314, 0.596078, 1.0);
    pub const PALE_TURQUOISE: Color = Color::new(0.686275, 0.933333, 0.933333, 1.0);
    pub const PALE_VIOLET_RED: Color = Color::new(0.858824, 0.439216, 0.576471, 1.0);
    pub const PAPAYA_WHIP: Color = Color::new(1.0, 0.937255, 0.835294, 1.0);
    pub const PEACH_PUFF: Color = Color::new(1.0, 0.854902, 0.72549, 1.0);
    pub const PERU: Color = Color::new(0.803922, 0.521569, 0.247059, 1.0);
    pub const PINK: Color = Color::new(1.0, 0.752941, 0.796078, 1.0);
    pub const PLUM: Color = Color::new(0.866667, 0.627451, 0.866667, 1.0);
    pub const POWDER_BLUE: Color = Color::new(0.690196, 0.878431, 0.901961, 1.0);
    pub const PURPLE: Color = Color::new(0.627451, 0.12549, 0.941176, 1.0);
    pub const REBECCA_PURPLE: Color = Color::new(0.4, 0.2, 0.6, 1.0);
    pub const RED: Color = Color::new(1.0, 0.0, 0.0, 1.0);
    pub const ROSY_BROWN: Color = Color::new(0.737255, 0.560784, 0.560784, 1.0);
    pub const ROYAL_BLUE: Color = Color::new(0.254902, 0.411765, 0.882353, 1.0);
    pub const SADDLE_BROWN: Color = Color::new(0.545098, 0.270588, 0.0745098, 1.0);
    pub const SALMON: Color = Color::new(0.980392, 0.501961, 0.447059, 1.0);
    pub const SANDY_BROWN: Color = Color::new(0.956863, 0.643137, 0.376471, 1.0);
    pub const SEA_GREEN: Color = Color::new(0.180392, 0.545098, 0.341176, 1.0);
    pub const SEASHELL: Color = Color::new(1.0, 0.960784, 0.933333, 1.0);
    pub const SIENNA: Color = Color::new(0.627451, 0.321569, 0.176471, 1.0);
    pub const SILVER: Color = Color::new(0.752941, 0.752941, 0.752941, 1.0);
    pub const SKY_BLUE: Color = Color::new(0.529412, 0.807843, 0.921569, 1.0);
    pub const SLATE_BLUE: Color = Color::new(0.415686, 0.352941, 0.803922, 1.0);
    pub const SLATE_GRAY: Color = Color::new(0.439216, 0.501961, 0.564706, 1.0);
    pub const SNOW: Color = Color::new(1.0, 0.980392, 0.980392, 1.0);
    pub const SPRING_GREEN: Color = Color::new(0.0, 1.0, 0.498039, 1.0);
    pub const STEEL_BLUE: Color = Color::new(0.27451, 0.509804, 0.705882, 1.0);
    pub const TAN: Color = Color::new(0.823529, 0.705882, 0.54902, 1.0);
    pub const TEAL: Color = Color::new(0.0, 0.501961, 0.501961, 1.0);
    pub const THISTLE: Color = Color::new(0.847059, 0.74902, 0.847059, 1.0);
    pub const TOMATO: Color = Color::new(1.0, 0.388235, 0.278431, 1.0);
    pub const TRANSPARENT: Color = Color::new(1.0, 1.0, 1.0, 0.0);
    pub const TURQUOISE: Color = Color::new(0.25098, 0.878431, 0.815686, 1.0);
    pub const VIOLET: Color = Color::new(0.933333, 0.509804, 0.933333, 1.0);
    pub const WEB_GRAY: Color = Color::new(0.501961, 0.501961, 0.501961, 1.0);
    pub const WEB_GREEN: Color = Color::new(0.0, 0.501961, 0.0, 1.0);
    pub const WEB_MAROON: Color = Color::new(0.501961, 0.0, 0.0, 1.0);
    pub const WEB_PURPLE: Color = Color::new(0.501961, 0.0, 0.501961, 1.0);
    pub const WHEAT: Color = Color::new(0.960784, 0.870588, 0.701961, 1.0);
    pub const WHITE: Color = Color::new(1.0, 1.0, 1.0, 1.0);
    pub const WHITE_SMOKE: Color = Color::new(0.960784, 0.960784, 0.960784, 1.0);
    pub const YELLOW: Color = Color::new(1.0, 1.0, 0.0, 1.0);
    pub const YELLOW_GREEN: Color = Color::new(0.603922, 0.803922, 0.196078, 1.0);

    /// New color, every value should be between 0-1
    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self(Vec4::new(r, g, b, a))
    }

    /// New color from rgb values between 0-255 for `r`,`g` and `b`.
    pub fn from_rgb(r: u8, g: u8, b: u8) -> Self {
        Self::from_rgba(r, g, b, 1.0)
    }

    /// New color from rgba values between 0-255 for `r`,`g` and `b`. Alpha `a` should be between 0-1
    pub fn from_rgba(r: u8, g: u8, b: u8, a: f32) -> Self {
        Self::new(
            f32::from(r) / 255.0,
            f32::from(g) / 255.0,
            f32::from(b) / 255.0,
            a.clamp(0.0, 1.0),
        )
    }

    /// New color from hsv. Hue `h` should be between 0-360. Saturation `s` between 0-1. Value `v` between 0-1.
    pub fn from_hsv(h: f32, s: f32, v: f32) -> Self {
        Self::from_hsva(h, s, v, 1.0)
    }

    /// New color from hsva. Hue `h` should be between 0-360. Saturation `s` between 0-1. Value `v` between 0-1. Alpha `a` between 0-1.
    pub fn from_hsva(h: f32, s: f32, v: f32, a: f32) -> Self {
        let hue = h.rem_euclid(360.0) / 360.0;
        let saturation = s.clamp(0.0, 1.0);
        let value = v.clamp(0.0, 1.0);
        let alpha = a.clamp(0.0, 1.0);

        let i = (hue * 6.0).floor();
        let f = hue * 6.0 - i;
        let p = value * (1.0 - saturation);
        let q = value * (1.0 - f * saturation);
        let t = value * (1.0 - (1.0 - f) * saturation);

        match i as u32 {
            0 => Self::new(value, t, p, alpha),
            1 => Self::new(q, value, p, alpha),
            2 => Self::new(p, value, t, alpha),
            3 => Self::new(p, q, value, alpha),
            4 => Self::new(t, p, value, alpha),
            _ => Self::new(value, p, q, alpha),
        }
    }

    /// New color from hsl. Hue `h` should be between 0-360. Saturation `s` between 0-1. Luminosity `l` between 0-1
    pub fn from_hsl(h: f32, s: f32, l: f32) -> Self {
        Self::from_hsla(h, s, l, 1.0)
    }

    /// New color from hsl. Hue `h` should be between 0-360. Saturation `s` between 0-1. Luminosity `l` between 0-1. Alpha `a` between 0-1.
    pub fn from_hsla(h: f32, s: f32, l: f32, a: f32) -> Self {
        // https://www.baeldung.com/cs/convert-color-hsl-rgb#method

        let chroma = (1.0 - (2.0 * l - 1.0).abs()) * s;
        let h_prime = h.clamp(0.0, 360.0) / 60.0; // H' (hue sector)
        let x = chroma * (1.0 - (h_prime.rem_euclid(2.0) - 1.0).abs());

        let (r1, g1, b1) = match h_prime {
            0.0..=1.0 => (chroma, x, 0.0),
            1.0..=2.0 => (x, chroma, 0.0),
            2.0..=3.0 => (0.0, chroma, x),
            3.0..=4.0 => (0.0, x, chroma),
            4.0..=5.0 => (x, 0.0, chroma),
            5.0..=6.0 => (chroma, 0.0, x),
            _ => (0.0, 0.0, 0.0),
        };

        let m = l - chroma / 2.0; // Match lightness
        Color::new(r1 + m, g1 + m, b1 + m, a)
    }

    /// Red. Between 0-1
    pub fn r(&self) -> f32 {
        self.x
    }

    /// Green. Between 0-1
    pub fn g(&self) -> f32 {
        self.y
    }

    /// Blue. Between 0-1
    pub fn b(&self) -> f32 {
        self.z
    }

    /// Alpha. Between 0-1
    pub fn a(&self) -> f32 {
        self.w
    }

    /// Red. Between 0-255
    pub fn r8(&self) -> u8 {
        (self.x * 255.0).clamp(0.0, 255.0).round() as u8
    }

    /// Green. Between 0-255
    pub fn g8(&self) -> u8 {
        (self.y * 255.0).clamp(0.0, 255.0).round() as u8
    }

    /// Blue. Between 0-255
    pub fn b8(&self) -> u8 {
        (self.z * 255.0).clamp(0.0, 255.0).round() as u8
    }

    /// Alpha. Between 0-255
    pub fn a8(&self) -> u8 {
        (self.w * 255.0).clamp(0.0, 255.0).round() as u8
    }

    /// Get the Hue of the current color
    pub fn hue(&self) -> f32 {
        let r = self.r();
        let g = self.g();
        let b = self.b();

        let max = r.max(g).max(b);
        let min = r.min(g).min(b);
        let c = max - min;

        let hue = if c != 0.0 {
            if max == r {
                let segment = (g - b) / c;
                let shift = if segment < 0.0 {
                    // R° / (360° / hex sides)
                    // hue > 180, full rotation
                    360 / 60 // R° / (360° / hex sides)
                } else {
                    0
                };
                segment + shift as f32
            } else if max == g {
                let segment = (b - r) / c;
                let shift = 120 / 60; // G° / (360° / hex sides)
                segment + shift as f32
            } else {
                let segment = (r - g) / c;
                let shift = 240 / 60; // B° / (360° / hex sides)
                segment + shift as f32
            }
        } else {
            0.0
        };

        hue * 60.0
    }

    /// Darken the current color by the amount. Amount must be between 0-1.
    /// Does not affect alpha.
    pub fn darken(&mut self, amount: f32) {
        let clamped = 1.0 - amount.clamp(0.0, 1.0);
        let mut as_glam = **self;
        as_glam.x *= clamped;
        as_glam.y *= clamped;
        as_glam.z *= clamped;
        self.0 = as_glam;
    }

    /// Lighten the current color by the amount. Amount must be between 0-1.
    /// Does not affect alpha.
    pub fn lighten(&mut self, amount: f32) {
        let clamped = amount.clamp(0.0, 1.0);
        let mut as_glam = **self;
        as_glam.x = as_glam.x + (1.0 - as_glam.x) * clamped;
        as_glam.y = as_glam.y + (1.0 - as_glam.y) * clamped;
        as_glam.z = as_glam.z + (1.0 - as_glam.z) * clamped;
        self.0 = as_glam;
    }

    /// Blend the current color with a new color equally on every channel.
    pub fn blend(&self, color: Color) -> Color {
        let as_glam = **self;
        let r = (as_glam.x + color.r()) / 2.0;
        let g = (as_glam.y + color.g()) / 2.0;
        let b = (as_glam.z + color.b()) / 2.0;
        let a = (as_glam.w + color.a()) / 2.0;
        Color::new(r, g, b, a)
    }

    pub fn to_hex_str(&self) -> String {
        format!(
            "#{:02X}{:02X}{:02X}{:02X}",
            self.r8(),
            self.g8(),
            self.b8(),
            self.a8()
        )
    }
}

impl From<Vec4> for Color {
    fn from(rgba: Vec4) -> Self {
        Self(rgba)
    }
}

impl From<[f32; 4]> for Color {
    fn from(rgba: [f32; 4]) -> Self {
        Self(Into::<glam::Vec4>::into(rgba).into())
    }
}

impl From<[f32; 3]> for Color {
    fn from(rgb: [f32; 3]) -> Self {
        Color::new(rgb[0], rgb[1], rgb[2], 1.0)
    }
}

impl From<&EventColor> for Color {
    fn from(value: &EventColor) -> Self {
        Color::new(value.r(), value.g(), value.b(), value.a())
    }
}

impl From<u32> for Color {
    /// Alpha must be included
    fn from(hex: u32) -> Self {
        let r = f32::from(((hex >> 24) & 0xFF) as u8) / 255.0;
        let g = f32::from(((hex >> 16) & 0xFF) as u8) / 255.0;
        let b = f32::from(((hex >> 8) & 0xFF) as u8) / 255.0;
        let a = f32::from((hex & 0xFF) as u8) / 255.0;

        Color::new(r, g, b, a)
    }
}

impl From<Color> for u32 {
    fn from(c: Color) -> Self {
        // Combine the components into a single u32 value (RGBA format)
        ((c.r8() as u32) << 24) | ((c.g8() as u32) << 16) | ((c.b8() as u32) << 8) | (c.a8() as u32)
    }
}

impl From<Color> for event::graphics::Color {
    fn from(value: Color) -> Self {
        Self::new(value.r(), value.g(), value.b(), value.a())
    }
}

#[derive(Debug, PartialEq)]
pub enum HexError {
    ParseError(ParseIntError),
    InvalidLength,
}

impl TryFrom<&str> for Color {
    type Error = HexError;

    fn try_from(hex: &str) -> Result<Self, Self::Error> {
        let code = hex.trim_start_matches('#');

        match code.len() {
            3 => {
                let rr = &code.chars().nth(0).unwrap();
                let gg = &code.chars().nth(1).unwrap();
                let bb = &code.chars().nth(2).unwrap();

                let r = try_str_to_num(&format!("{rr}{rr}"))?;
                let g = try_str_to_num(&format!("{gg}{gg}"))?;
                let b = try_str_to_num(&format!("{bb}{bb}"))?;
                Ok(Self::new(r, g, b, 1.0))
            }
            6 => {
                let r = try_str_to_num(&code[0..2])?;
                let g = try_str_to_num(&code[2..4])?;
                let b = try_str_to_num(&code[4..6])?;
                Ok(Self::new(r, g, b, 1.0))
            }
            8 => {
                let r = try_str_to_num(&code[0..2])?;
                let g = try_str_to_num(&code[2..4])?;
                let b = try_str_to_num(&code[4..6])?;
                let a = try_str_to_num(&code[6..8])?;
                Ok(Self::new(r, g, b, a))
            }
            _ => Err(HexError::InvalidLength),
        }
    }
}

impl Deref for Color {
    type Target = Vec4;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Color {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rgb() {
        assert_eq!(Color::from_rgb(255, 255, 255), Color::WHITE);
        assert_eq!(Color::from_rgb(0, 0, 0), Color::BLACK);
        assert_eq!(Color::from_rgb(0, 255, 0), Color::LIME);
        assert_eq!(Color::from_rgb(0, 0, 255), Color::BLUE);
        assert_eq!(Color::from_rgb(255, 0, 0), Color::RED);
        assert_eq!(Color::from_rgb(0, 255, 255), Color::CYAN);
        // pastel blue
        assert_eq!(
            Color::from_rgb(102, 153, 204),
            Color::new(0.4, 0.6, 0.8, 1.0)
        );
    }

    #[test]
    fn rgba() {
        assert_eq!(Color::from_rgba(255, 255, 255, 1.0), Color::WHITE);
    }

    #[test]
    fn r8() {
        assert_eq!(Color::new(0.5, 0.0, 0.0, 0.0).r8(), 128);
    }

    #[test]
    fn g8() {
        assert_eq!(Color::new(0.0, 0.5, 0.0, 0.0).g8(), 128);
    }

    #[test]
    fn b8() {
        assert_eq!(Color::new(0.0, 0.0, 0.5, 0.0).b8(), 128);
    }

    #[test]
    fn a8() {
        assert_eq!(Color::new(0.0, 0.0, 0.0, 0.5).a8(), 128);
    }

    #[test]
    fn hsv() {
        assert_eq!(Color::from_hsv(0.0, 0.0, 0.0), Color::BLACK);
        assert_eq!(Color::from_hsv(0.0, 0.0, 1.0), Color::WHITE);
        assert_eq!(Color::from_hsv(0.0, 1.0, 1.0), Color::RED);
        assert_eq!(Color::from_hsv(60.0, 1.0, 1.0), Color::YELLOW);
        assert_eq!(Color::from_hsv(120.0, 1.0, 1.0), Color::LIME);
        assert_eq!(Color::from_hsv(240.0, 1.0, 1.0), Color::BLUE);
        assert_eq!(Color::from_hsv(300.0, 1.0, 1.0), Color::MAGENTA);
        assert_eq!(
            Color::from_hsv(210.0, 0.5, 0.8),
            Color::try_from("6699cc").unwrap()
        );
        assert_eq!(
            Color::from_hsv(90.0, 0.5, 0.8),
            Color::try_from("99cc66").unwrap()
        );
        assert_eq!(
            Color::from_hsv(60.0, 1.0, 0.8),
            Color::try_from("CCCC00").unwrap()
        );
        assert_eq!(Color::from_hsv(60.0, 1.0, 0.8), Color::from(0xCCCC00FF));
    }

    #[test]
    fn hue() {
        assert_eq!(Color::BLACK.hue(), 0.0);
        assert_eq!(Color::WHITE.hue(), 0.0);
        assert_eq!(Color::RED.hue(), 0.0);
        assert_eq!(Color::YELLOW.hue(), 60.0);
        assert_eq!(Color::LIME.hue(), 120.0);
        assert_eq!(Color::CYAN.hue(), 180.0);
        assert_eq!(Color::BLUE.hue(), 240.0);
        assert_eq!(Color::MAGENTA.hue(), 300.0);
        assert_eq!(Color::from_rgb(170, 120, 243).hue(), 264.39026);
    }

    #[test]
    fn hsl() {
        assert_eq!(Color::from_hsl(0.0, 0.0, 0.0), Color::BLACK);
        assert_eq!(Color::from_hsl(0.0, 0.0, 1.0), Color::WHITE);
        assert_eq!(Color::from_hsl(0.0, 1.0, 0.5), Color::RED);
        assert_eq!(Color::from_hsl(120.0, 1.0, 0.5), Color::LIME);
        assert_eq!(Color::from_hsl(240.0, 1.0, 0.5), Color::BLUE);
        assert_eq!(
            Color::from_hsla(0.0, 0.0, 0.0, 0.0),
            Color::new(0.0, 0.0, 0.0, 0.0)
        );
        // light gray
        assert_eq!(
            Color::from_hsl(0.0, 0.0, 0.8),
            Color::try_from("cccccc").unwrap()
        );
        assert_eq!(Color::from_hsl(0.0, 1.0, 0.5), Color::RED);
    }

    #[test]
    fn hex() {
        // a very nice mild green color
        assert_eq!(
            Color::try_from("2dbd54").unwrap(),
            Color::from_rgb(45, 189, 84)
        );
        assert_eq!(
            Color::try_from("6699cc").unwrap(),
            Color::from_rgb(102, 153, 204)
        );
        assert_eq!(Color::try_from("000").unwrap(), Color::BLACK);
        assert_eq!(Color::try_from("000000").unwrap(), Color::BLACK);
        assert_eq!(
            Color::try_from("00000000").unwrap(),
            Color::new(0.0, 0.0, 0.0, 0.0)
        );
        assert_eq!(Color::try_from("#000").unwrap(), Color::BLACK);
        assert_eq!(Color::try_from("#000000").unwrap(), Color::BLACK);
        assert_eq!(
            Color::try_from("#00000000").unwrap(),
            Color::new(0.0, 0.0, 0.0, 0.0)
        );
        assert_eq!(Color::try_from("ffffff").unwrap(), Color::WHITE);
        assert_eq!(Color::try_from("fff").unwrap(), Color::WHITE);
        assert_eq!(Color::try_from("ff0000").unwrap(), Color::RED);
        assert_eq!(Color::try_from("f00").unwrap(), Color::RED);
        assert_eq!(Color::try_from("00ff00").unwrap(), Color::LIME);
        assert_eq!(Color::try_from("0f0").unwrap(), Color::LIME);
        assert_eq!(Color::try_from("0000ff").unwrap(), Color::BLUE);
        assert_eq!(Color::try_from("00f").unwrap(), Color::BLUE);
        assert_eq!(Color::try_from("ffff00").unwrap(), Color::YELLOW);
        assert_eq!(Color::try_from("ff0").unwrap(), Color::YELLOW);
        assert_eq!(
            Color::try_from("abcdefghijklmn"),
            Err(HexError::InvalidLength)
        );
    }

    #[test]
    fn hex_str() {
        assert_eq!(Color::WHITE.to_hex_str(), "#FFFFFFFF");
        assert_eq!(Color::BLACK.to_hex_str(), "#000000FF");
        assert_eq!(Color::RED.to_hex_str(), "#FF0000FF");
        assert_eq!(Color::LIME.to_hex_str(), "#00FF00FF");
    }

    #[test]
    fn hex_int() {
        assert_eq!(Color::from(0xFFFFFFFF).to_hex_str(), "#FFFFFFFF");
        assert_eq!(Color::from(0x000000FF).to_hex_str(), "#000000FF");
        assert_eq!(Color::from(0xFF0000FF).to_hex_str(), "#FF0000FF");
        assert_eq!(Color::from(0x00FF00FF).to_hex_str(), "#00FF00FF");
        assert_eq!(Color::from(0xFFFFFF00).to_hex_str(), "#FFFFFF00");
        assert_eq!(Color::from(0x00000000).to_hex_str(), "#00000000");
        assert_eq!(Color::from(0xFF000000).to_hex_str(), "#FF000000");
        assert_eq!(Color::from(0x00FF0000).to_hex_str(), "#00FF0000");
        assert_eq!(Color::from(0xCCCC00FF).to_hex_str(), "#CCCC00FF");
        assert_eq!(Color::from(0xCCCC00FF), Color::from_rgb(204, 204, 0));
    }

    #[test]
    fn darken() {
        let mut color = Color::from(0xFFFFFFFF);
        color.darken(0.5);
        assert_eq!(color.to_hex_str(), "#808080FF");
    }

    #[test]
    fn lighten() {
        let mut color = Color::from(0x000000);
        color.lighten(0.5);
        assert_eq!(color.to_hex_str(), "#80808000");
    }

    #[test]
    fn blend() {
        assert_eq!(Color::WHITE.blend(Color::BLACK).to_hex_str(), "#808080FF");
    }

    #[test]
    fn into() {
        assert_eq!(u32::from(Color::new(0.0, 0.0, 0.0, 0.0)), 0x00000000);
        assert_eq!(u32::from(Color::new(1.0, 1.0, 1.0, 1.0)), 0xFFFFFFFF);
    }
}
