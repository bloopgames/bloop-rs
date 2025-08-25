use std::{fmt::Debug, iter::zip};

use super::*;

fn there_and_back_again<T: Serialize + Deserialize>(val: &T) -> T {
    // Serialize.
    let mut serializer = Serializer::new(Vec::new());
    val.serialize(&mut serializer).unwrap();
    let serialized = serializer.into_writer();

    // Deserialize.
    let mut deserializer = Deserializer::new(serialized.as_slice());
    unsafe { T::deserialize(&mut deserializer).unwrap() }
}

fn there_and_back_in_place<T: Serialize + Deserialize>(mut val: T) -> T {
    // Serialize.
    let mut serializer = Serializer::new(Vec::new());
    val.serialize(&mut serializer).unwrap();
    let serialized = serializer.into_writer();

    // Deserialize.
    let mut deserializer = Deserializer::new(serialized.as_slice());
    unsafe {
        val.deserialize_in_place(&mut deserializer).unwrap();
    }
    val
}

/// Tests both deserialization and in-place deserialization.
#[allow(clippy::needless_pass_by_value)]
fn there_and_back_both<T: Serialize + Deserialize + Clone + Eq + Debug>(val: T) {
    let res = there_and_back_again(&val);
    assert_eq!(val, res);

    let res = there_and_back_in_place(val.clone());
    assert_eq!(val, res);
}

#[test]
fn serialize_bool() {
    there_and_back_both(true);
}

#[test]
fn serialize_int() {
    there_and_back_both(123_u32);
}

#[test]
fn serialize_usize() {
    there_and_back_both(123_usize);
}

#[test]
fn serialize_option() {
    there_and_back_both(None::<u32>);
    there_and_back_both(Some(123_u32));
}

#[test]
fn serialize_array() {
    there_and_back_both([1, 2, 3]);
}

#[test]
fn serialize_boxed_bytes() {
    let bytes = [
        MaybeUninit::new(1),
        MaybeUninit::new(2),
        MaybeUninit::new(3),
    ]
    .into();

    let res = there_and_back_again::<Box<[MaybeUninit<u8>]>>(&bytes);

    for (a, b) in zip(bytes, res) {
        unsafe {
            assert_eq!(a.assume_init(), b.assume_init());
        }
    }
}

#[test]
fn serialize_string() {
    there_and_back_both(String::from("test"));
}

#[test]
fn serialize_vec_pod() {
    there_and_back_both(vec![1, 2, 3]);
}

#[test]
fn serialize_vec() {
    there_and_back_both(vec![
        String::from("test1"),
        String::from("test2"),
        String::from("test3"),
    ]);
}

#[test]
fn serialize_avec() {
    let bytes = [
        MaybeUninit::new(1),
        MaybeUninit::new(2),
        MaybeUninit::new(3),
    ];

    let mut vec = AVec::new(16);
    vec.extend_from_slice(&bytes);

    let res = there_and_back_again(&vec);

    for (a, b) in zip(bytes, res.as_slice()) {
        unsafe {
            assert_eq!(a.assume_init(), b.assume_init());
        }
    }
}

#[test]
fn serialize_map() {
    there_and_back_both(HashMap::from([
        (String::from("test1"), 1),
        (String::from("test2"), 2),
        (String::from("test3"), 3),
    ]));
}

#[test]
fn serialize_multiple() {
    let map = HashMap::from([
        (String::from("test1"), 1),
        (String::from("test2"), 2),
        (String::from("test3"), 3),
    ]);

    let vec = vec![
        String::from("test1"),
        String::from("test2"),
        String::from("test3"),
    ];

    let array = [1, 2, 3];

    let mut serializer = Serializer::new(Vec::new());

    // Serialize.
    map.serialize(&mut serializer).unwrap();
    vec.serialize(&mut serializer).unwrap();
    array.serialize(&mut serializer).unwrap();

    let serialized = serializer.into_writer();

    let mut deserializer = Deserializer::new(serialized.as_slice());

    // Deserialize.
    assert_eq!(map, unsafe {
        HashMap::deserialize(&mut deserializer).unwrap()
    });
    assert_eq!(vec, unsafe {
        Vec::<String>::deserialize(&mut deserializer).unwrap()
    });
    assert_eq!(array, unsafe {
        <[i32; 3]>::deserialize(&mut deserializer).unwrap()
    });
}
