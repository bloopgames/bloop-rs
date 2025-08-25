use std::{
    array::from_fn,
    fmt::Debug,
    ops::{Index, IndexMut},
};

use engine_derive::resource;
use glam::Vec2;
use snapshot_derive::{DeserializeEngine, SerializeEngine};

use crate::{
    ComponentId, EcsType, Resource,
    event::input::{KeyCode, MouseButton},
};

#[repr(C)]
#[derive(Debug, Default)]
#[resource(engine)]
pub struct InputState {
    pub keys: KeyboardInputState,
    pub mouse: MouseState,
}

/// Stores the button state for all supported keycodes.
#[repr(C)]
#[derive(DeserializeEngine, SerializeEngine)]
pub struct KeyboardInputState(pub [ButtonState; KeyCode::ENUM_VALUES.len()]);

impl Default for KeyboardInputState {
    fn default() -> Self {
        Self(from_fn(|_| Default::default()))
    }
}

impl Debug for KeyboardInputState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("KeyboardInputState");

        for (i, state) in self.0.iter().enumerate() {
            f.field(KeyCode(i as u8).variant_name().unwrap(), state);
        }

        f.finish()
    }
}

impl Index<KeyCode> for KeyboardInputState {
    type Output = ButtonState;

    fn index(&self, index: KeyCode) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<KeyCode> for KeyboardInputState {
    fn index_mut(&mut self, index: KeyCode) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

impl KeyboardInputState {
    pub fn iter(&self) -> impl Iterator<Item = (KeyCode, &ButtonState)> {
        self.0
            .iter()
            .enumerate()
            .map(|(i, state)| (KeyCode(i as u8), state))
    }
}

#[repr(C)]
#[derive(Debug, Default, DeserializeEngine, SerializeEngine)]
pub struct MouseState {
    /// Cursor position, in pixels.
    pub cursor_position: Vec2,
    /// Mouse scroll change since previous frame. Y is the standard axis.
    pub scroll_delta: Vec2,

    pub buttons: MouseButtonState,
}

#[repr(C)]
#[derive(Default, DeserializeEngine, SerializeEngine)]
pub struct MouseButtonState(pub [ButtonState; MouseButton::ENUM_VALUES.len()]);

impl Debug for MouseButtonState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("MouseButtonState");

        for (i, state) in self.0.iter().enumerate() {
            f.field(MouseButton(i as u8).variant_name().unwrap(), state);
        }

        f.finish()
    }
}

impl Index<MouseButton> for MouseButtonState {
    type Output = ButtonState;

    fn index(&self, index: MouseButton) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<MouseButton> for MouseButtonState {
    fn index_mut(&mut self, index: MouseButton) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

impl MouseButtonState {
    pub fn iter(&self) -> impl Iterator<Item = (MouseButton, &ButtonState)> {
        self.0
            .iter()
            .enumerate()
            .map(|(i, state)| (MouseButton(i as u8), state))
    }
}

/// Stores the history of a button's state. A `0` bit signifies that the button is not
/// pressed, and a `1` bit signifies that the button is pressed. The LSB represents the
/// current frame's state, and each frame the state is shifted left by one bit.
#[repr(transparent)]
#[derive(Clone, Copy, Default, DeserializeEngine, SerializeEngine)]
pub struct ButtonState(pub u8);

impl ButtonState {
    /// Returns `true` if the button is currently pressed.
    pub fn pressed(&self) -> bool {
        (0b01 & self.0) == 0b01
    }

    /// Returns `true` if the button was just pressed this frame.
    pub fn just_pressed(&self) -> bool {
        (0b11 & self.0) == 0b01
    }

    /// Returns `true` if the button was just released this frame.
    pub fn just_released(&self) -> bool {
        (0b11 & self.0) == 0b10
    }

    /// Sets the LSB.
    pub fn set_pressed(&mut self, pressed: bool) {
        self.0 = (self.0 & !1) | pressed as u8;
    }
}

impl Debug for ButtonState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ButtonState")
            .field("pressed", &self.pressed())
            .field("just_pressed", &self.just_pressed())
            .field("just_released", &self.just_released())
            .finish()
    }
}
