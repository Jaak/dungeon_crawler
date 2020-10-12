use crate::error::Result;

use cuckoofilter::{CuckooFilter};
use std::collections::hash_map::DefaultHasher;
use std::{fs::File, path};
use std::io::prelude::*;
use std::io::{BufReader, BufWriter};
use tempfile::NamedTempFile;
use log::{error, info};
use fs2::FileExt;

// Deduplicate rows (possibly imperfectly) of a given text file.
// The file in given path will be overwritten by deduplicated file.
// Note that with a low probability the result may still have some
// duplicate rows.
// TODO: deduplication should happen automatically when extending an CSV file!
fn dedup_rows(path: path::PathBuf) -> Result<()> {
    let file = File::open(&path)?;
    // NOTE: Lock will be released when the file handle is closed
    file.try_lock_exclusive()?;
    let temp = NamedTempFile::new()?;
    temp.as_file().try_lock_exclusive()?;
    let mut dropped = 0;
    let mut lines = 0;
    let mut filter = CuckooFilter::<DefaultHasher>::new();
    let mut writer = BufWriter::new(&temp);
    for line in BufReader::new(file).lines() {
        let line = line?;
        let line_u8 = line.as_bytes();
        if filter.test_and_add(line_u8)? {
            writer.write_all(line_u8)?;
            writer.write_all(b"\n")?;
        } else {
            dropped += 1;
        }

        lines += 1;
    }

    if lines == 0 || dropped == 0 {
        info!("No lines removed from '{}'", path.display());
    }
    else {
        let rate = 100.0 * (dropped as f32 / lines as f32);
        info!("Removed {}% lines from '{}'", rate.round(), path.display());
    }

    drop(writer);
    temp.persist(path)?;
    Ok(())
}

pub fn run_dedup(paths: Vec<path::PathBuf>) -> Result<()> {
    crossbeam::scope(|scope| {
        for path in paths {
            if ! path.is_file() {
                error!("ERROR: '{}' is not a file. Skipping.", path.display());
                continue;
            }

            scope.spawn(move |_| {
                if let Err(err) = dedup_rows(path) {
                    error!("ERROR: {}", err);
                }
            });
        };
    }).unwrap();

    Ok(())
}