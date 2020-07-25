extern crate csv;
extern crate curl;
extern crate env_logger;
extern crate log;
extern crate regex;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate static_assertions;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate serde_derive;
extern crate chrono;
extern crate crossbeam;
extern crate dotenv;
#[macro_use]
extern crate crossbeam_channel;
extern crate structopt;
extern crate tempfile;
extern crate cuckoofilter;
extern crate fs2;
extern crate governor;
#[macro_use]
extern crate nonzero_ext;
#[macro_use]
extern crate anyhow;
extern crate futures;

mod error;
mod dedup;
mod download;
mod download_async;
mod json;
mod summary_data;

use dotenv::dotenv;
use std::{str, path};
use structopt::StructOpt;
use error::Result;

#[derive(StructOpt)]
#[structopt(
    name = "dungeon-crawler",
    about = "Download World of Warcraft mythic+ leaderboard data",
    version = "0.3.0",
    author = "Jaak Randmets <jaak.ra@gmail.com>",
    rename_all = "kebab-case")]
enum Cfg {
    #[structopt(name = "download", about = "Download leaderboard data for a given week")]
    Download(download::DownloadCmd),

    #[structopt(name = "new-download", about = "Download leaderboard data for a given week")]
    NewDownload(download::DownloadCmd),

    #[structopt(name = "dedup", about = "Deduplicate a CSV file rows")]
    Dedup {
        #[structopt(help="CSV files to deduplicate.", parse(from_os_str))]
        paths: Vec<path::PathBuf>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LOG", "dungeon_crawler");
    env_logger::init();
    dotenv().ok();

    match Cfg::from_args() {
        Cfg::Dedup{paths} => dedup::run_dedup(paths)?,
        Cfg::Download(cmd) => download::run_download(cmd)?,
        Cfg::NewDownload(cmd) => download_async::async_download(cmd).await?,
    }

    Ok(())
}