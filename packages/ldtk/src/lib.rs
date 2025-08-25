use std::{iter::zip, path::PathBuf};

use anyhow::{Error, Result};
use engine::prelude::*;
use serde::Deserialize;
use serde_json::Value;

pub struct Ldtk {
    parent_dir: PathBuf,
    root: LdtkRoot,
}

#[derive(Deserialize)]
pub struct LdtkRoot {
    #[serde(alias = "jsonVersion")]
    json_version: String,
    defs: LdtkDefs,
    levels: Vec<LdtkLevel>,
}

#[derive(Deserialize)]
pub struct LdtkDefs {
    layers: Vec<serde_json::Map<String, Value>>,
    tilesets: Vec<LdtkTileset>,
}

#[derive(Deserialize)]
pub struct LdtkTileset {
    uid: u64,
    #[serde(alias = "relPath")]
    rel_path: Option<String>,
    #[serde(alias = "pxWid")]
    px_wid: u64,
    #[serde(alias = "pxHei")]
    px_hei: u64,
}

#[derive(Deserialize)]
pub struct LdtkLevel {
    pub identifier: String,
    #[serde(alias = "pxWid")]
    pub px_wid: u64,
    #[serde(alias = "pxHei")]
    pub px_hei: u64,
    #[serde(alias = "bgColor")]
    pub bg_color: String,
    #[serde(alias = "layerInstances")]
    layer_instances: Vec<Value>,
}

pub struct Entity<'a> {
    pub identifier: &'a str,
    pub position: Vec2,
    pub size: Vec2,
    /// Values from `[0, 1]`.
    pub pivot: Vec2,
}

pub struct AutoLayerTile {
    pub bounds: Rect,
    pub uv_region: Rect,
    pub a: f32,
}

pub struct IntGridTile<'a> {
    pub bounds: Rect,
    pub color: Color,
    pub identifier: Option<&'a str>,
}

impl Ldtk {
    /// - `parent_dir_path`: The directory containing the .ldtk file, relative
    ///   to the `assets/` folder.
    /// - `file_contents`: the loaded ldtk json file.
    ///
    /// # TODO
    ///
    /// Eventually this function will take a single asset path parameter,
    /// once the engine supports loading files from disk.
    pub fn open(parent_dir_path: &str, file_contents: &str) -> Result<Self> {
        let root: LdtkRoot = serde_json::from_str(file_contents)?;

        if root.json_version != "1.5.3" {
            return Err(Error::msg("only supports LDtk version 1.5.3"));
        }

        Ok(Self {
            parent_dir: parent_dir_path.into(),
            root,
        })
    }

    pub fn level(&self, level_identifier: &str) -> Option<&LdtkLevel> {
        self.root
            .levels
            .iter()
            .find(|layer| layer.identifier == level_identifier)
    }

    /// Returns an iterator over all entities in an Entities layer.
    ///
    /// # Errors
    ///
    /// Returns an error if the layer is not found or if the layer is not of
    /// type `Entities`.
    pub fn entities(
        &self,
        level_identifier: &str,
        layer_identifier: &str,
    ) -> Result<impl Iterator<Item = Entity<'_>>> {
        let layer_instance = self.layer_instance(level_identifier, layer_identifier)?;

        if layer_instance["__type"].as_str().unwrap() != "Entities" {
            return Err(Error::msg("layer is not type `Entities`"));
        }

        let entities = layer_instance["entityInstances"]
            .as_array()
            .unwrap()
            .iter()
            .map(|entity| {
                let px = entity["px"].as_array().unwrap();
                let position = Vec2::new(
                    px[0].as_f64().unwrap() as f32,
                    -px[1].as_f64().unwrap() as f32,
                );

                let size = Vec2::new(
                    entity["width"].as_f64().unwrap() as f32,
                    entity["height"].as_f64().unwrap() as f32,
                );

                let pivot = entity["__pivot"].as_array().unwrap();
                let pivot = Vec2::new(
                    pivot[0].as_f64().unwrap() as f32,
                    pivot[1].as_f64().unwrap() as f32,
                );

                Entity {
                    identifier: entity["__identifier"].as_str().unwrap(),
                    position,
                    size,
                    pivot,
                }
            });

        Ok(entities)
    }

    /// Returns the path of the tilemap used by this `AutoLayer`, if it exists.
    ///
    /// # Errors
    ///
    /// Returns an error if the layer is not found or if the layer is not an
    /// `AutoLayer`.
    pub fn auto_layer_tilemap_path(
        &self,
        level_identifier: &str,
        layer_identifier: &str,
    ) -> Result<Option<String>> {
        let layer_instance = self.layer_instance(level_identifier, layer_identifier)?;

        if layer_instance["__type"].as_str().unwrap() != "AutoLayer" {
            return Err(Error::msg("layer is not type `AutoLayer`"));
        }

        let tileset_def_uid = layer_instance["__tilesetDefUid"].as_u64().unwrap();
        let tileset_def = self
            .root
            .defs
            .tilesets
            .iter()
            .find(|tileset| tileset.uid == tileset_def_uid)
            .unwrap();

        let path = tileset_def.rel_path.as_ref().map(|rel_path| {
            // Create a path relative to `assets/`.
            let path = self.parent_dir.join(rel_path);
            path.into_os_string().into_string().unwrap()
        });

        Ok(path)
    }

    /// Returns an iterator over all tiles in the `AutoLayer`.
    ///
    /// # Errors
    ///
    /// Returns an error if the layer is not found or if the layer is not an
    /// `AutoLayer`.
    pub fn auto_layer_tiles(
        &self,
        level_identifier: &str,
        layer_identifier: &str,
    ) -> Result<impl Iterator<Item = AutoLayerTile>> {
        let layer_instance = self.layer_instance(level_identifier, layer_identifier)?;

        if layer_instance["__type"].as_str().unwrap() != "AutoLayer" {
            return Err(Error::msg("layer is not type `AutoLayer`"));
        }

        let grid_size = layer_instance["__gridSize"].as_f64().unwrap() as f32;

        let tileset_def_uid = layer_instance["__tilesetDefUid"].as_u64().unwrap();
        let tileset_def = self
            .root
            .defs
            .tilesets
            .iter()
            .find(|tileset| tileset.uid == tileset_def_uid)
            .unwrap();

        let uv_w = grid_size / tileset_def.px_wid as f32;
        let uv_h = grid_size / tileset_def.px_hei as f32;

        let tiles = layer_instance["autoLayerTiles"]
            .as_array()
            .unwrap()
            .iter()
            .map(move |tile| {
                let px = tile["px"].as_array().unwrap();

                let src = tile["src"].as_array().unwrap();
                let uv_x = src[0].as_f64().unwrap() as f32 / tileset_def.px_wid as f32;
                let uv_y = src[1].as_f64().unwrap() as f32 / tileset_def.px_hei as f32;

                let a = tile["a"].as_f64().unwrap() as f32;

                AutoLayerTile {
                    bounds: Rect {
                        position: Vec2::new(
                            px[0].as_f64().unwrap() as f32,
                            -px[1].as_f64().unwrap() as f32,
                        ),
                        dimensions: Vec2::splat(grid_size),
                    },
                    uv_region: Rect::new(uv_x, uv_y, uv_w, uv_h),
                    a,
                }
            });

        Ok(tiles)
    }

    /// Returns an iterator over all tiles in the `IntGrid`.
    ///
    /// # Errors
    ///
    /// Returns an error if the layer is not found or if the layer is not an
    /// `IntGrid`.
    pub fn int_grid_tiles(
        &self,
        level_identifier: &str,
        layer_identifier: &str,
    ) -> Result<impl Iterator<Item = IntGridTile<'_>>> {
        let layer_instance = self.layer_instance(level_identifier, layer_identifier)?;

        if layer_instance["__type"].as_str().unwrap() != "IntGrid" {
            return Err(Error::msg("layer is not type `IntGrid`"));
        }

        let def_uid = &layer_instance["layerDefUid"];
        let layer_def = self
            .root
            .defs
            .layers
            .iter()
            .find(|def| def["uid"] == *def_uid)
            .unwrap();

        let width = layer_instance["__cWid"].as_u64().unwrap();
        let height = layer_instance["__cHei"].as_u64().unwrap();
        let grid_size = layer_instance["__gridSize"].as_u64().unwrap();
        let dimensions = Vec2::splat(grid_size as f32);

        let int_grid_values = layer_def["intGridValues"].as_array().unwrap();

        let coords = (0..height).flat_map(move |y| (0..width).map(move |x| [x, y]));
        let grid_indices = layer_instance["intGridCsv"].as_array().unwrap();

        let tiles = zip(coords, grid_indices)
            .filter(|(_, grid_index)| grid_index.as_u64().unwrap() > 0)
            .map(move |(coord, grid_index)| {
                let position = *Vec2::new(coord[0] as f32, -(coord[1] as f32)) * *dimensions;

                let grid_value = int_grid_values
                    .iter()
                    .find(|value| value["value"] == *grid_index)
                    .unwrap();

                let color = grid_value["color"].as_str().unwrap();
                let color = Color::try_from(color).unwrap();

                IntGridTile {
                    bounds: Rect {
                        position: position.into(),
                        dimensions,
                    },
                    color,
                    identifier: grid_value["identifier"].as_str(),
                }
            });

        Ok(tiles)
    }

    fn layer_instance(&self, level_identifier: &str, layer_identifier: &str) -> Result<&Value> {
        let Some(level) = self
            .root
            .levels
            .iter()
            .find(|layer| layer.identifier == level_identifier)
        else {
            return Err(Error::msg(format!("level `{level_identifier}` not found")));
        };

        let Some(layer_instance) = level
            .layer_instances
            .iter()
            .find(|layer| layer["__identifier"] == layer_identifier)
        else {
            return Err(Error::msg(format!("layer `{layer_identifier}` not found")));
        };

        Ok(layer_instance)
    }
}
