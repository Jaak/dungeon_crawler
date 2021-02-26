use crate::{error::*, json};
use crate::summary_data::Region;
use std::path;

use serde_json::Value;
use structopt::StructOpt;
use chrono::prelude::*;
use log::{error, info};
use std::{env, process, str, collections::HashSet, fs, sync::Arc};
use hyper_tls::HttpsConnector;
use tokio::sync::Mutex;
use hyper::{body::HttpBody as _, Client, Request, Body};
use fs2::FileExt;
use regex::Regex;
use std::{rc::Rc, time::{self, Duration}, cell::RefCell};
use governor::{Quota, state, clock::DefaultClock};
use anyhow::Context;
use futures::stream::{FuturesUnordered, StreamExt};

#[derive(StructOpt)]
pub struct GetCmd {
    #[structopt(long, default_value = "eu", help="Region to download from. Either eu, us, tw, kr or cn.")]
    pub region: Region,

    #[structopt(short = "o", long, help="Output CSV file.", parse(from_os_str))]
    pub output: Option<path::PathBuf>,

    #[structopt(long, short = "n", default_value = "dynamic", help="BattleNet namespave.")]
    pub namespace: String,

    #[structopt(help="API endpoint.")]
    pub api: String,
}

type AsyncClient = Client<HttpsConnector<hyper::client::connect::HttpConnector>>;

struct RequestCtx {
    access_token: String,
    region: Region,
    namespace: String,
}

impl RequestCtx {
    fn bearer_str(&self) -> String {
        format!("Bearer {token}", token = self.access_token)
    }

    fn query_str(&self, query: &str) -> String {
        format!(
            "{gateway}/{query}?namespace={namespace}-{region}&locale=en_US",
            gateway = self.region.gateway_uri(),
            region = self.region.to_string(),
            namespace = self.namespace,
            query = query
        )
    }
}

fn build_request(ctx: &RequestCtx, query: &str) -> Request<Body> {
    let uri = &ctx.query_str(query);
    dbg!(uri);
    Request::get(uri)
        .header("Authorization", ctx.bearer_str())
        .body(Body::empty())
        .unwrap()
}

// fn build_post()

async fn async_token_request(client: &AsyncClient, region: Region) -> Result<json::AccessToken> {
    let client_id = &env::var("CLIENT_ID")?;
    let client_secret = &env::var("CLIENT_SECRET")?;
    let url : String = format!(
        "{uri}?grant_type=client_credentials&client_id={client_id}&client_secret={client_secret}",
        uri=region.token_uri(),
        client_id=client_id,
        client_secret=client_secret
    ).parse()?;

    let mut data = Vec::new();
    let req = Request::post(url).body(Body::empty())?;
    let mut resp = client.request(req).await?;
    while let Some(next) = resp.body_mut().data().await {
        let chunk = next?;
        data.extend_from_slice(&chunk);
    }

    serde_json::from_slice(data.as_slice()).with_context(||
        format!("Failed to parse json string: \"{}\"",
            String::from_utf8(data).unwrap_or_default())
    )
}

async fn async_query(client: AsyncClient, ctx: RequestCtx, query: &str) -> Result<Value>
{
    let req = build_request(&ctx, query);

    let timeout_msg = Vec::from("Timeout".as_bytes());
    let server_error_msg = Vec::from("Internal Server Error".as_bytes());

    let mut data = Vec::new();
    let mut resp = client.request(req).await?;
    while let Some(next) = resp.body_mut().data().await {
        let chunk = next?;
        data.extend_from_slice(&chunk);
    }

    if data.is_empty() { bail!("Received empty response"); }
    if data == timeout_msg { bail!("Request timed out."); }
    if data == server_error_msg { bail!("Internal server error."); }
    serde_json::from_slice(data.as_slice()).with_context(|| {
        format!("Failed to parse json string: \"{}\" in respose to query \"{}\"",
            String::from_utf8(data).unwrap_or_default(), query)
    })
}

pub async fn run(cmd: GetCmd) -> Result<()> {
    let GetCmd{region, output, namespace, api} = cmd;
    let https = HttpsConnector::new();
    let client = Client::builder()
        .build::<_, hyper::Body>(https);
    let access_token = async_token_request(&client, region).await
        .context("Failed access token request.")?;
    println!("got token!");
    let ctx = RequestCtx { region, namespace, access_token: access_token.access_token };
    let res = async_query(client, ctx, api.as_str()).await?;
    println!("{}", serde_json::to_string_pretty(&res)?);
    Ok(())
}