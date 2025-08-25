//! EXPERIMENTAL DST TYPES.
//!
//! In leu of the engine providing a stable-ABI `Vec` and `String`, this is a
//! place to experiment with what these types may look like. The long-term plan
//! is to establish a canonical engine DST ABI.
//!
//! These types are not (yet) safe to use in Components, as this would result in
//! memory leaks. As a result, these types do not implement `Copy`.

pub mod option;
pub mod string;
pub mod vec;
