# Bloop - Rust API

All the packages needed to develop Bloop games in Rust.

# Documentation

https://docs.trybloop.gg/rust/

# Usage

Include any packages needed by your project as git links in your project's `Cargo.toml`:

```toml
[dependencies]
engine = { git = "https://github.com/bloopgames/bloop-rs.git", tag = "0.0.22" }
```

Using a tag will ensure that the version of `bloop-rs` packages in use are compatible with your matching version of the engine.

**Patch version changes are currently considered breaking.**
