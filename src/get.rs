use crate::{error::*, json};
use crate::summary_data::Region;
use std::{fs::File, path};

use serde_json::Value;
use structopt::StructOpt;
use hyper_tls::HttpsConnector;
use hyper::{body::HttpBody as _, Client, Request, Body};
use std::io::BufWriter;
use std::{env, str};
use anyhow::Context;

#[derive(StructOpt)]
pub struct GetCmd {
    #[structopt(long, default_value = "eu", help="Region to download from. Either eu, us, tw, kr or cn.")]
    pub region: Region,

    #[structopt(short = "o", long, help="Output CSV file.", parse(from_os_str))]
    pub output: Option<path::PathBuf>,

    #[structopt(long, short = "n", default_value = "dynamic", help="BattleNet namespace.")]
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
    Request::get(uri)
        .header("Authorization", ctx.bearer_str())
        .body(Body::empty())
        .unwrap()
}

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

    if namespace != "static" && namespace != "dynamic" && namespace != "profile" {
        bail!("Invalid namespace. Either \"static\", \"dynamic\" or \"profile\".")
    }

    let https = HttpsConnector::new();
    let client = Client::builder()
        .build::<_, hyper::Body>(https);
    let access_token = async_token_request(&client, region).await
        .context("Failed access token request.")?;
    let ctx = RequestCtx { region, namespace, access_token: access_token.access_token };
    let res = async_query(client, ctx, api.as_str()).await?;
    if let Some(output) = output {
        let file = File::create(output)?;
        let writer = BufWriter::new(file);
        serde_json::to_writer_pretty(writer, &res)?;
    }
    else {
        println!("{}", serde_json::to_string_pretty(&res)?);
    }
    Ok(())
}