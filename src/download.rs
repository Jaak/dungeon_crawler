use crate::error::*;
use crate::summary_data::{Dungeon, Region, DataRow};
use crate::json;

use chrono::prelude::*;
use crossbeam_channel::{bounded, tick, TrySendError, RecvError};
use curl::easy::{List, Easy};
use fs2::FileExt;
use log::{error, info};
use regex::Regex;
use std::{cell::RefCell, path, env, process, fs, collections::HashSet, thread, time};
use structopt::StructOpt;

#[derive(StructOpt)]
pub struct DownloadCmd {
    #[structopt(long, default_value = "eu", help="Region to download from. Either eu, us, tw, kr or cn.")]
    pub region: Region,

    #[structopt(long, default_value = "10", help="Number of worker threads to spawn.")]
    pub workers: usize,

    #[structopt(long, default_value = "10", help="Request rate limit (per second).")]
    pub rate: f32,

    #[structopt(long, help="Time period ID. Use latest time period as default.")]
    pub period: Option<u32>,

    #[structopt(short = "o", long, help="Output CSV file.", parse(from_os_str))]
    pub output: Option<path::PathBuf>,

    #[structopt(help="Period index CSV file.", default_value = "data/static/period-index.csv", parse(from_os_str))]
    pub period_index_file: path::PathBuf,
}

struct Ctx {
    access_token: String,
    region: Region,
    easy: RefCell<Easy>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, PartialOrd)]
#[serde(rename_all = "PascalCase")]
struct PeriodIndexEntry {
    pub region: Region,
    #[serde(rename = "PeriodId")]
    pub id: u32,
    pub start_timestamp: u64,
    pub end_timestamp: u64,
}

#[derive(Copy, Clone)]
struct GetMythicLeaderboard {
    connected_realm_id: u32,
    dungeon_id: u32,
    period: u32,
}

fn write_to_vector(easy: &mut Easy) -> Result<Vec<u8>> {
    let mut result = Vec::new();
    {
        let mut transfer = easy.transfer();
        transfer.write_function(|data| {
            result.extend_from_slice(data);
            Ok(data.len())
        })?;

        transfer.perform()?;
    }

    Ok(result)
}

fn query<T>(ctx: &Ctx, query: &str) -> Result<T>
where
    T: serde::de::DeserializeOwned
{
    if let Some(res) = try_query(ctx, query)? {
        return Ok(res);
    }

    bail!("Received no bytes as the result of the query.");
}

fn try_query<T>(ctx: &Ctx, query: &str) -> Result<Option<T>>
where
    T: serde::de::DeserializeOwned
{
    let mut easy = ctx.easy.borrow_mut();
    let query_str = &format!(
        "{gateway}/{query}?namespace=dynamic-{region}&locale=en_US",
        gateway = ctx.region.gateway_uri(),
        region = ctx.region.to_string(),
        query = query
    );
    easy.url(query_str)?;

    let mut list = List::new();
    let header_str = &format!("Authorization: Bearer {token}", token = ctx.access_token);
    list.append(header_str)?;
    easy.http_headers(list)?;

    let timeout_msg = Vec::from("Timeout".as_bytes());
    for i in 1 .. 10 {
        let data = write_to_vector(&mut easy)?;
        if data == timeout_msg {
            error!("Timeout nr. {} of query \"{}\"", i, query_str);
            continue;
        }

        if data.is_empty() {
            return Ok(None);
        }

        match serde_json::from_slice(data.as_slice()) {
            Ok(json_value) => {
                if i > 1 {
                    info!("Query \"{}\" that previously timed out is OK", query_str);
                }

                return Ok(json_value);
            }
            Err(err) => {
                error!("Failed to parse json string ({} bytes): \"{}\"",
                    data.len(), String::from_utf8(data)?);
                error!("In response to query \"{}\"", query);
                Err(err)?
            }
        }
    }

    bail!("Timeout error");
}

// Resulting token is just printed out to stdout.
fn token_request(easy: &mut Easy, region: Region) -> Result<json::AccessToken> {
    let client_id = &env::var("CLIENT_ID")?;
    let client_secret = &env::var("CLIENT_SECRET")?;
    let url = &format!(
        "{uri}?grant_type=client_credentials&client_id={client_id}&client_secret={client_secret}",
        uri=region.token_uri(),
        client_id=client_id,
        client_secret=client_secret
    );

    easy.url(url)?;
    let mut list = List::new();
    list.append("Accept: application/json")?;
    easy.http_headers(list)?;
    let data = write_to_vector(easy)?;
    match serde_json::from_slice(data.as_slice()) {
        Ok(json_value) => Ok(json_value),
        Err(err) => {
            error!("Failed to parse json string: {}", String::from_utf8(data)?);
            Err(err)?
        }
    }
}

fn query_connected_realms(ctx: &Ctx) -> Result<Vec<u32>> {
    lazy_static! {
        static ref MATCH_REALM: Regex =
            Regex::new(r"/data/wow/connected-realm/(?P<connectedRealmId>\d+)").unwrap();
    }

    let realm_index: json::ConnectedRealmIndex =
        query(ctx, "data/wow/connected-realm/index")?;
    let mut result = Vec::new();
    for href in realm_index.connected_realms {
        for c in MATCH_REALM.captures_iter(&href.href) {
            if let Some(val) = c.name("connectedRealmId") {
                result.push(val.as_str().parse()?);
            }
        }
    }

    Ok(result)
}

#[derive(Debug)]
struct LeaderboardEntry {
    dungeon_name: Dungeon,
    dungeon_id: u32,
}

fn query_mythic_leaderboard_index(ctx: &Ctx, realm_id: u32) -> Result<Vec<LeaderboardEntry>> {
    let query_str = &format!(
        "data/wow/connected-realm/{connectedRealmId}/mythic-leaderboard/index",
        connectedRealmId = realm_id);
    let leaderboard: json::LeaderboardIndex = query(ctx, query_str)?;
    let mut result = Vec::new();
    for entry in leaderboard.current_leaderboards {
        if let Some(dungeon) = Dungeon::from_id(entry.id) {
            result.push(LeaderboardEntry {
                dungeon_name: dungeon,
                dungeon_id: entry.id,
            });
        }
        else {
            bail!("Unexpected dungeon id {} for \"{}\"", entry.id, entry.name);
        }
    }

    Ok(result)
}

fn query_mythic_leaderboard(ctx: &Ctx, q: &GetMythicLeaderboard) -> Result<json::Leaderboard> {
    let query_str = &format!(
        "data/wow/connected-realm/{connectedRealmId}/mythic-leaderboard/{dungeonId}/period/{period}",
        connectedRealmId = q.connected_realm_id,
        dungeonId = q.dungeon_id,
        period = q.period);
    query(ctx, query_str)
}

fn query_period_index(ctx: &Ctx) -> Result<json::PeriodIndex> {
    let query_str = "data/wow/mythic-keystone/period/index";
    query(ctx, query_str)
}

fn query_period(ctx: &Ctx, id: u32) -> Result<json::Period> {
    let query_str = &format!("data/wow/mythic-keystone/period/{}", id);
    query(ctx, query_str)
}

// Configure curl to accept compression and use HTTP2 .
fn configure_easy(easy: &mut Easy) -> Result<()> {
    easy.accept_encoding("gzip, deflate")?;
    easy.pipewait(true)?;
    Ok(())
}

fn update_period_index(ctx: &Ctx,
                       period_index: &json::PeriodIndex,
                       path: path::PathBuf) -> Result<()>
{
    if ! path.is_file() {
        error!("ERROR: Period index '{}' is missing (or not a file). Skipping.", path.display());
        return Ok(());
    }

    let mut indices : HashSet<u32> = period_index.periods.iter().map(|x| x.id).collect();
    {
        let mut rdr = csv::ReaderBuilder::new().delimiter(b';').from_path(&path)?;
        for record in rdr.deserialize() {
            let record: PeriodIndexEntry = record?;
            if record.region != ctx.region {
                continue;
            }

            indices.remove(&record.id);
        }
    }

    if indices.is_empty() {
        info!("Period index file '{}' is up to date for {} region.",
              path.display(), ctx.region.to_string());
        return Ok(());
    }

    info!("Updating period index '{}' for {} region.", path.display(), ctx.region.to_string());

    let file = fs::OpenOptions::new()
        .append(true).write(true).create(false).open(&path)?;
    file.try_lock_exclusive()?;
    let mut wrt = csv::WriterBuilder::new()
        .has_headers(false).delimiter(b';').from_writer(file);

    for period_id in indices {
        let period = query_period(ctx, period_id)?;
        wrt.serialize(PeriodIndexEntry {
            region: ctx.region,
            id: period.id,
            start_timestamp: period.start_timestamp,
            end_timestamp: period.end_timestamp,
        })?;
    }

    Ok(())
}

pub fn run_download(cmd: DownloadCmd) -> Result<()> {

    info!("Requesting token...");
    let mut easy = Easy::new();
    configure_easy(&mut easy)?;
    let DownloadCmd{region, workers, rate, period, output, period_index_file} = cmd;
    let access_token = &token_request(&mut easy, region)?;

    // Global context
    let gctx = &Ctx {
        access_token: access_token.access_token.clone(),
        region: region,
        easy: RefCell::new(easy),
    };

    // Query period ID index (TODO: cache this based on the latest time range)
    info!("query_period_index()...");
    let period_index = query_period_index(gctx)?;
    let period_id = match period {
        None => period_index.current_period.id,
        Some(id) => {
            if period_index.periods.iter().find(|index| index.id == id).is_none() {
                error!("Invalid period index.");
                process::exit(1);
            }

            id
        }
    };

    update_period_index(gctx, &period_index, period_index_file)?;

    info!("Querying period info (period_id = {})", period_id);
    let period_info = query_period(gctx, period_id)?;
    let path = match output {
        Some(path) => path,
        None => {
            let dt = Utc.timestamp_millis(period_info.start_timestamp as i64);
            let csv_file_name = format!(
                "{}-leaderboard-{}.csv",
                dt.format("%Y-%m-%d"),
                region.to_string()
            );

            path::PathBuf::from(csv_file_name)
        }
    };

    let exists = path.exists();
    if exists &&  ! path.is_file() {
        error!("\'{}\' is not a file.", path.display());
        process::exit(1);
    }

    info!("{} leaderboard to {}",
        if exists { "Appending" } else { "Writing" },
        path.display());

    let create = ! exists;
    let file = fs::OpenOptions::new()
        .append(true).write(true).create(create).open(&path)?;

    let mut wrt = csv::WriterBuilder::new()
        .delimiter(b';').has_headers(create).from_writer(file);

    let mut guards = Vec::new();
    {
        // Main thread sends queries to worker threads:
        let (queries_s, queries_r) = bounded(workers);

        // Worker threads send CSV rows to CSV writer thread:
        let (rows_s, rows_r) = bounded(workers);

        // Spawn worker threads
        for _ in 1..workers {
            let mut easy = Easy::new();
            configure_easy(&mut easy)?;

            let ctx = Ctx {
                access_token: access_token.access_token.clone(),
                region: region,
                easy: RefCell::new(easy),
            };

            let rows_s = rows_s.clone();
            let handle_query = move |q| -> Result<()> {
                let leaderboard = query_mythic_leaderboard(&ctx, &q)?;
                let dungeon = Dungeon::from_id(q.dungeon_id).unwrap();
                if let Some(leading_groups) = leaderboard.leading_groups {
                    for leading_group in leading_groups {
                        rows_s.send(DataRow::new(region, dungeon, &leading_group))?;
                    }
                }

                Ok(())
            };

            let queries_r = queries_r.clone();
            let guard = thread::spawn(move || -> Result<()> {
                // Note to self. Following can be done with a "while let" loop.
                // But following has better clarity as to what is going on.
                loop {
                    match queries_r.recv() {
                        Ok(q) => handle_query(q)?,
                        // Following only happens when channel is empty and disconnected:
                        Err(RecvError) => return Ok(()),
                    }
                }
            });

            guards.push(guard);
        }

        // Ticker to flush the CSV file every second:
        let ticker = tick(time::Duration::from_secs(1));

        // Spawn thread for writing CSV file
        let guard = thread::spawn(move || -> Result<()> {
            loop {
                select! {
                    recv(ticker) -> _ => wrt.flush()?,
                    recv(rows_r) -> row => match row {
                        Ok(row) => wrt.serialize(row)?,
                        Err(_) => {
                            // This means that rows_r has been closed.
                            break;
                        },
                    },
                }
            };

            Ok(())
        });

        guards.push(guard);

        info!("Gathering connected realm ID-s...");
        let realms = query_connected_realms(gctx)?;
        let inst = time::Instant::now();
        let sleep_dur = time::Duration::from_millis((1000f32/rate) as _);
        let mut queries_sent = 0;

        info!("Querying all leaderboards...");
        for connected_realm in realms {
            let realm_leaderboard_index = query_mythic_leaderboard_index(gctx, connected_realm)?;
            for index in realm_leaderboard_index {
                let q = GetMythicLeaderboard {
                    connected_realm_id: connected_realm,
                    dungeon_id: index.dungeon_id,
                    period: period_id,
                };

                // TODO: think about the rate limiting algorithm little bit more
                loop {
                    thread::sleep(sleep_dur);
                    match queries_s.try_send(q) {
                        Err(TrySendError::Full(_)) => continue,
                        Err(TrySendError::Disconnected(_)) =>
                            bail!("Unexpected disconnect of a send-channel (queries_s)."),
                        Ok(()) => break,
                    }
                };

                queries_sent += 1;
            }
        }

        let dur = inst.elapsed().as_secs();
        info!(
            "Done! Sent {} leaderboard queries in {} seconds ({} queries per seconds).",
            queries_sent, dur, if dur > 0 { queries_sent / dur } else { 0 }
        );
    }

    for guard in guards {
        match guard.join().unwrap() {
            Ok(()) => {}
            Err(err) => {
                error!("Worker thread error on closing: {}", err);
            }
        };
    }

    Ok(())
}