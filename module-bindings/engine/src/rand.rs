use engine_derive::resource;
use rand::{RngCore, SeedableRng};
use rand_pcg::Pcg32;

use crate::{ComponentId, EcsType, Resource};

#[repr(transparent)]
#[resource(engine)]
pub struct EngineRng(RngInner);

impl Default for EngineRng {
    fn default() -> Self {
        // Seed with 0. The expectation is that the platform will reseed `Rng`
        // with an os-provided seed before any game code runs.
        Self(RngInner(Pcg32::seed_from_u64(0)))
    }
}

impl RngCore for EngineRng {
    fn next_u32(&mut self) -> u32 {
        self.0.0.next_u32()
    }

    fn next_u64(&mut self) -> u64 {
        self.0.0.next_u64()
    }

    fn fill_bytes(&mut self, dst: &mut [u8]) {
        self.0.0.fill_bytes(dst);
    }
}

/// A struct implementing `snapshot` traits.
#[repr(transparent)]
struct RngInner(Pcg32);

impl snapshot::Serialize for RngInner {
    fn serialize<W>(&self, serializer: &mut snapshot::Serializer<W>) -> snapshot::Result<()>
    where
        W: snapshot::WriteUninit,
    {
        // Internally, `Pcg32` is just two `u64` fields.
        let rng = unsafe { std::mem::transmute::<Pcg32, [u64; 2]>(self.0.clone()) };
        serializer.serialize_pod(&rng)
    }
}

impl snapshot::Deserialize for RngInner {
    unsafe fn deserialize<R>(deserializer: &mut snapshot::Deserializer<R>) -> snapshot::Result<Self>
    where
        R: snapshot::ReadUninit,
    {
        unsafe {
            let rng: [u64; 2] = deserializer.deserialize_pod()?;
            let rng = std::mem::transmute::<[u64; 2], Pcg32>(rng);
            Ok(Self(rng))
        }
    }
}
