use crate::download::DownloadCmd;
use crate::error::Result;
use crate::json;
use crate::summary_data::{Dungeon, Region, DataRow};

use chrono::prelude::*;
use log::{error, info};
use std::{env, process, str, path, collections::HashSet, fs, sync::Arc};
use hyper_tls::HttpsConnector;
use tokio::sync::Mutex;
use hyper::{body::HttpBody as _, Client, Request, Body};
use fs2::FileExt;
use regex::Regex;
use std::{rc::Rc, time::{self, Duration}, cell::RefCell};
use governor::{Quota, state, clock::DefaultClock};
use anyhow::Context;
use futures::stream::{FuturesUnordered, StreamExt};

type AsyncClient = Client<HttpsConnector<hyper::client::connect::HttpConnector>>;

type RateLimiter = governor::RateLimiter<state::NotKeyed, state::InMemoryState, DefaultClock>;

struct Ctx {
    access_token: String,
    region: Region,
    client: AsyncClient,
    limiter: RateLimiter,
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

fn build_request(ctx: &Ctx, query: &str) -> Request<Body> {
    let query_str = &format!(
        "{gateway}/{query}?namespace=dynamic-{region}&locale=en_US",
        gateway = ctx.region.gateway_uri(),
        region = ctx.region.to_string(),
        query = query
    );

    let bearer_str = &format!("Bearer {token}", token = ctx.access_token);

    Request::get(query_str)
        .header("Authorization", bearer_str)
        .body(Body::empty())
        .unwrap()
}

/**
 * Handle request while trying to respect the rate limit.
 * TODO: implement retry mechanism.
 *
 */
async fn handle_request(ctx: Arc<Ctx>, req: Request<Body>) -> Result<Vec<u8>> {
    let timeout_msg = Vec::from("Timeout".as_bytes());
    let server_error_msg = Vec::from("Internal Server Error".as_bytes());

    let small_jitter = governor::Jitter::up_to(Duration::from_millis(1));
    ctx.limiter.until_ready_with_jitter(small_jitter).await;
    let mut data = Vec::new();
    let mut resp = ctx.client.request(req).await?;
    while let Some(next) = resp.body_mut().data().await {
        let chunk = next?;
        data.extend_from_slice(&chunk);
    }

    if data.is_empty() {
        bail!("Received empty response");
    }

    if data == timeout_msg {
        bail!("Request timed out.");
    }

    if data == server_error_msg {
        bail!("Internal server error.");
    }

    return Ok(data);
}

async fn async_query<T>(ctx: Arc<Ctx>, query: &str) -> Result<T>
where
    T: serde::de::DeserializeOwned,
{
    let req = build_request(&ctx, query);
    let data = handle_request(ctx, req).await.with_context(||
        format!("Failed to handle request in response to query \"{}\"", query)
    )?;
    serde_json::from_slice(data.as_slice()).with_context(|| {
        format!("Failed to parse json string: \"{}\" in respose to query \"{}\"",
            String::from_utf8(data).unwrap_or_default(), query)
    })
}

#[derive(Debug)]
struct LeaderboardEntry {
    dungeon_name: Dungeon,
    dungeon_id: u32,
}

async fn query_mythic_leaderboard_index(ctx: Arc<Ctx>, realm_id: u32) -> Result<Vec<LeaderboardEntry>> {
    let query_str = &format!(
        "data/wow/connected-realm/{connectedRealmId}/mythic-leaderboard/index",
        connectedRealmId = realm_id);
    let leaderboard: json::LeaderboardIndex = async_query(ctx, query_str).await?;
    let mut result = Vec::new();
    for entry in leaderboard.current_leaderboards {
        if let Some(dungeon) = Dungeon::from_id(entry.id) {
            result.push(LeaderboardEntry {
                dungeon_name: dungeon,
                dungeon_id: entry.id,
            });
        }
        else {
            bail!("Unexpected dungeon id {} for \"{}\"", entry.id, entry.name)
        }
    }

    Ok(result)
}

async fn query_mythic_leaderboard(ctx: Arc<Ctx>, q: &GetMythicLeaderboard) -> Result<json::Leaderboard> {
    let query_str = &format!(
        "data/wow/connected-realm/{connectedRealmId}/mythic-leaderboard/{dungeonId}/period/{period}",
        connectedRealmId = q.connected_realm_id,
        dungeonId = q.dungeon_id,
        period = q.period);
    async_query(ctx, query_str).await
}

async fn query_period_index(ctx: Arc<Ctx>) -> Result<json::PeriodIndex> {
    let query_str = "data/wow/mythic-keystone/period/index";
    async_query(ctx, query_str).await
}

async fn query_period(ctx: Arc<Ctx>, id: u32) -> Result<json::Period> {
    let query_str = &format!("data/wow/mythic-keystone/period/{}", id);
    async_query(ctx, query_str).await
}

async fn query_connected_realms(ctx: Arc<Ctx>) -> Result<Vec<u32>> {
    lazy_static! {
        static ref MATCH_REALM: Regex =
            Regex::new(r"/data/wow/connected-realm/(?P<connectedRealmId>\d+)").unwrap();
    }

    let realm_index: json::ConnectedRealmIndex =
        async_query(ctx, "data/wow/connected-realm/index").await?;
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

async fn async_token_request(client: &AsyncClient, region: Region) -> Result<json::AccessToken> {
    let client_id = &env::var("CLIENT_ID")?;
    let client_secret = &env::var("CLIENT_SECRET")?;
    let url = format!(
        "{uri}?grant_type=client_credentials&client_id={client_id}&client_secret={client_secret}",
        uri=region.token_uri(),
        client_id=client_id,
        client_secret=client_secret
    ).parse()?;

    let mut data = Vec::new();
    let mut resp = client.get(url).await?;
    while let Some(next) = resp.body_mut().data().await {
        let chunk = next?;
        data.extend_from_slice(&chunk);
    }

    serde_json::from_slice(data.as_slice()).with_context(||
        format!("Failed to parse json string: \"{}\"",
            String::from_utf8(data).unwrap_or_default())
    )
}

async fn update_period_index(
    ctx: Arc<Ctx>,
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

    let wrt = Rc::new(RefCell::new(csv::WriterBuilder::new()
        .has_headers(false).delimiter(b';').from_writer(file)));

    let mut queries = FuturesUnordered::new();

    for period_id in indices {
        let ctx = ctx.clone();
        let wrt = wrt.clone();
        queries.push(async move {
            let region = ctx.region;
            let period = query_period(ctx, period_id).await?;
            let entry = PeriodIndexEntry {
                region: region,
                id: period.id,
                start_timestamp: period.start_timestamp,
                end_timestamp: period.end_timestamp,
            };

            wrt.borrow_mut().serialize(entry)?;
            Result::Ok(())
        });
    }

    while let Some(maybe_result) = queries.next().await {
        if let Err(err) = maybe_result {
            error!("Skipped a query: {:?}", err);
            continue;
        }
    }

    Ok(())
}

pub async fn async_download(cmd: DownloadCmd) -> Result<()> {
    let DownloadCmd{region, workers: _, rate: _, period, output, period_index_file} = cmd;
    let https = HttpsConnector::new();
    let client = Client::builder()
        .keep_alive(true)
        .build::<_, hyper::Body>(https);
    let access_token = async_token_request(&client, region).await
        .context("Failed access token request.")?;

    let limiter = RateLimiter::direct(Quota::per_second(nonzero!(10u32)));
    let ctx = Arc::new(Ctx {
        access_token: access_token.access_token.clone(),
        region,
        client,
        limiter,
    });

    // Query period ID index
    info!("Querying period index...");
    let period_index = query_period_index(ctx.clone()).await?;
    let period_id = match period {
        None => period_index.current_period.id,
        Some(id) => {
            if period_index.periods.iter().find(|index| index.id == id).is_none() {
                bail!("Invalid period index.");
            }

            id
        }
    };

    info!("Updating period index...");
    update_period_index(ctx.clone(), &period_index, period_index_file).await?;

    info!("Querying info for period ID {}", period_id);
    let period_info = query_period(ctx.clone(), period_id).await?;
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
        .append(true).write(true).create(create).open(path)?;
    let wrt = csv::WriterBuilder::new().delimiter(b';').has_headers(create).from_writer(file);
    let wrt = Arc::new(Mutex::new(wrt));

    info!("Gathering connected realm ID-s...");
    let inst = time::Instant::now();
    let realms = query_connected_realms(ctx.clone()).await?;

    let mut queries = FuturesUnordered::new();
    for connected_realm in realms {
        let ctx = ctx.clone();
        queries.push(async move {
            let indices =
                query_mythic_leaderboard_index(ctx, connected_realm).await
                .with_context(||
                    format!("Skipped leaderboards for realm ID {}", connected_realm)
                )?
                .into_iter()
                .map(|index|
                    GetMythicLeaderboard {
                        connected_realm_id: connected_realm,
                        dungeon_id: index.dungeon_id,
                        period: period_id,
                    }
                )
                .collect::<Vec<_>>();
            Result::Ok(indices)
        });
    }

    let mut handles = Vec::new();
    while let Some(maybe_queries) = queries.next().await {
        if let Err(err) = maybe_queries {
            error!("Skipped: {}", err);
            continue;
        }

        for q in maybe_queries.unwrap() {
            let wrt = wrt.clone();
            let ctx = ctx.clone();
            let handle = tokio::spawn(
                async move {
                    let leaderboard = query_mythic_leaderboard(ctx, &q).await?;
                    let dungeon = Dungeon::from_id(q.dungeon_id).expect("Unexpected dungeon ID!");
                    if let Some(leading_groups) = leaderboard.leading_groups {
                        for leading_group in leading_groups {
                            let row = DataRow::new(region, dungeon, &leading_group);
                            wrt.lock().await.serialize(row)?;
                        }
                    }

                    Result::Ok(())
                }
            );

            handles.push(handle);
        }
    }

    let queries_sent = handles.len();
    for handle in handles {
        match handle.await.expect("Join error.") {
            Ok(()) => {}
            Err(err) => {
                error!("Worker thread error: {:?}", err);
            }
        };
    }

    let dur = inst.elapsed().as_secs();
    info!(
        "Done! Sent {} leaderboard queries in {} seconds ({} queries per seconds).",
        queries_sent, dur, if dur > 0 { queries_sent as u64 / dur } else { 0 }
    );

    Ok(())
}