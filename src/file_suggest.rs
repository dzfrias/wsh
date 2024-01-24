use std::{env, fs};

use wsh_cmp::source::Source;

#[derive(Debug)]
pub struct FileSuggest;

impl Source for FileSuggest {
    fn suggest(&self) -> Vec<String> {
        let Ok(current_dir) = env::current_dir().and_then(fs::read_dir) else {
            return vec![];
        };

        current_dir
            .filter_map(|entry| {
                entry
                    .ok()?
                    .path()
                    .file_name()
                    .map(|name| name.to_string_lossy().to_string())
            })
            .collect()
    }
}
