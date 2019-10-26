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

use chrono::prelude::*;
use crossbeam_channel::{bounded, tick, TrySendError};
use cuckoofilter::{CuckooFilter};
use curl::easy::{Easy, List};
use dotenv::dotenv;
use fs2::FileExt;
use log::{error, info, warn};
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::str::FromStr;
use std::{env, error, process, str, thread, time, path, fs};
use std::io::prelude::*;
use std::io::{BufReader, BufWriter};
use std::fs::{File};
use structopt::StructOpt;
use tempfile::{NamedTempFile};

struct Ctx {
    access_token: String,
    region: Region,
    easy: RefCell<Easy>,
}

type Result<T> = std::result::Result<T, Box<dyn error::Error + Send + Sync>>;

#[derive(Debug, Serialize)]
enum TankSpecialization {
    ProtectionPaladin = 1,
    ProtectionWarrior,
    BloodDk,
    VengeanceDh,
    GuardianDruid,
    BrewmasterMonk,
}

#[derive(Debug, Serialize)]
enum HealerSpecialization {
    RestorationDruid = 1,
    MistweaverMonk,
    HolyPaladin,
    DisciplinePriest,
    HolyPriest,
    RestorationShaman,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, PartialOrd)]
enum Region {
    Eu = 1,
    Us,
    Kr,
    Tw,
    Cn
}

impl Region {
    pub fn gateway_uri(self) -> &'static str {
        match self {
            Region::Eu => "https://eu.api.blizzard.com/",
            Region::Us => "https://us.api.blizzard.com/",
            Region::Kr => "https://kr.api.blizzard.com/",
            Region::Tw => "https://tw.api.blizzard.com/",
            Region::Cn => "https://gateway.battlenet.com.cn/",
        }
    }

    pub fn token_uri(self) -> &'static str {
        match self {
            Region::Eu => "https://eu.battle.net/oauth/token",
            Region::Us => "https://us.battle.net/oauth/token",
            Region::Kr => "https://apac.battle.net/oauth/token",
            Region::Tw => "https://apac.battle.net/oauth/token",
            Region::Cn => "https://www.battlenet.com.cn/oauth/token",
        }
    }
}

impl ToString for Region {
    fn to_string(&self) -> String {
        match self {
            Region::Eu => "eu",
            Region::Us => "us",
            Region::Kr => "kr",
            Region::Tw => "tw",
            Region::Cn => "cn",
        }.to_string()
    }
}

impl FromStr for Region {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_ref() {
            "eu" => Ok(Region::Eu),
            "us" => Ok(Region::Us),
            "kr" => Ok(Region::Kr),
            "tw" => Ok(Region::Tw),
            "cn" => Ok(Region::Cn),
            _ => Err("Invalid region. Either eu, us, kr, tw or cn.")?
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize)]
enum Dungeon {
    AtalDazar = 1,
    Freehold,
    KingsRest,
    ShrineOfTheStorms,
    SiegeOfBoralus,
    TempleOfSethraliss,
    TheMotherlode,
    TheUnderrot,
    TolDagor,
    WaycrestManor,
}

impl FromStr for Dungeon {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Dungeon, Self::Err> {
        lazy_static! {
            static ref HASH_MAP: HashMap<&'static str, Dungeon> =
            [
                ("Atal'Dazar", Dungeon::AtalDazar),
                ("Freehold", Dungeon::Freehold),
                ("Tol Dagor", Dungeon::TolDagor),
                ("The MOTHERLODE!!", Dungeon::TheMotherlode),
                ("Waycrest Manor", Dungeon::WaycrestManor),
                ("Kings' Rest", Dungeon::KingsRest),
                ("Temple of Sethraliss", Dungeon::TempleOfSethraliss),
                ("The Underrot", Dungeon::TheUnderrot),
                ("Shrine of the Storm", Dungeon::ShrineOfTheStorms),
                ("Siege of Boralus", Dungeon::SiegeOfBoralus),
            ].iter().cloned().collect();
        }

        match HASH_MAP.get(s) {
            None => Err(format!("Bad dungeon name '{}'", s)),
            Some(dungeon) => Ok(*dungeon),
        }
    }
}

#[derive(Debug, Serialize)]
enum Faction {
    ALLIANCE = 1,
    HORDE,
}

// Make sure that Option<> optimization works correctly with our enums.
assert_eq_size!(Option<TankSpecialization>, TankSpecialization);
assert_eq_size!(Option<HealerSpecialization>, HealerSpecialization);
assert_eq_size!(Option<Region>, Region);
assert_eq_size!(Option<Dungeon>, Dungeon);
assert_eq_size!(Option<Faction>, Faction);

#[serde(rename_all = "PascalCase")]
#[derive(Default, Debug, Serialize)]
struct DataRow {
    region: Option<Region>,
    faction: Option<Faction>,
    dungeon: Option<Dungeon>,
    timestamp: u64,
    duration: u32,
    keystone_level: u32,
    // Group composition:
    tank: Option<TankSpecialization>,
    healer: Option<HealerSpecialization>,
    num_frost_dk: u8,
    num_unholy_dk: u8,
    num_havoc_dh: u8,
    num_balance_druid: u8,
    num_feral_druid: u8,
    num_beast_master_hunter: u8,
    num_marksmanship_hunter: u8,
    num_survival_hunter: u8,
    num_arcane_mage: u8,
    num_fire_mage: u8,
    num_frost_mage: u8,
    num_windwalker_monk: u8,
    num_retribution_paladin: u8,
    num_shadow_priest: u8,
    num_assassination_rogue: u8,
    num_outlaw_rogue: u8,
    num_subtlety_rogue: u8,
    num_elemental_shaman: u8,
    num_enhancement_shaman: u8,
    num_affliction_warlock: u8,
    num_demonology_warlock: u8,
    num_destruction_warlock: u8,
    num_arms_warrior: u8,
    num_fury_warrior: u8,
}

impl DataRow {
    pub fn new(region: Region, dungeon: Dungeon, group: &json::LeadingGroup) -> DataRow {
        let mut result = DataRow {
            region: Some(region),
            faction: None,
            dungeon: Some(dungeon),
            timestamp: group.completed_timestamp,
            duration: group.duration,
            keystone_level: group.keystone_level,
            ..Default::default()
        };

        for json_member in &group.members {
            result.update_group_composition(json_member.specialization.id);
            result.update_faction(&json_member.faction._type);
        }

        if result.faction.is_none() {
            warn!("Missing faction of a group.");
        }

        result
    }

    fn set_healer(&mut self, spec: HealerSpecialization) {
        if self.healer.is_none() {
            self.healer = Some(spec);
        }
    }

    fn set_tank(&mut self, spec: TankSpecialization) {
        if self.tank.is_none() {
            self.tank = Some(spec);
        }
    }

    fn update_faction(&mut self, faction_name: &str) {
        match faction_name.to_uppercase().as_str() {
            "ALLIANCE" => { self.faction.replace(Faction::ALLIANCE); },
            "HORDE" => { self.faction.replace(Faction::HORDE); },
            _ => {},
        }
    }

    fn update_group_composition(&mut self, id: u32) {
        match id {
            62 => self.num_arcane_mage += 1,
            63 => self.num_fire_mage += 1,
            64 => self.num_frost_mage += 1,
            65 => self.set_healer(HealerSpecialization::HolyPaladin),
            66 => self.set_tank(TankSpecialization::ProtectionPaladin),
            70 => self.num_retribution_paladin += 1,
            71 => self.num_arms_warrior += 1,
            72 => self.num_fury_warrior += 1,
            73 => self.set_tank(TankSpecialization::ProtectionWarrior),
            102 => self.num_balance_druid += 1,
            103 => self.num_feral_druid += 1,
            104 => self.set_tank(TankSpecialization::GuardianDruid),
            105 => self.set_healer(HealerSpecialization::RestorationDruid),
            250 => self.set_tank(TankSpecialization::BloodDk),
            251 => self.num_frost_dk += 1,
            252 => self.num_unholy_dk += 1,
            253 => self.num_beast_master_hunter += 1,
            254 => self.num_marksmanship_hunter += 1,
            255 => self.num_survival_hunter += 1,
            256 => self.set_healer(HealerSpecialization::DisciplinePriest),
            257 => self.set_healer(HealerSpecialization::HolyPriest),
            258 => self.num_shadow_priest += 1,
            259 => self.num_assassination_rogue += 1,
            260 => self.num_outlaw_rogue += 1,
            261 => self.num_subtlety_rogue += 1,
            262 => self.num_elemental_shaman += 1,
            263 => self.num_enhancement_shaman += 1,
            264 => self.set_healer(HealerSpecialization::RestorationShaman),
            265 => self.num_affliction_warlock += 1,
            266 => self.num_demonology_warlock += 1,
            267 => self.num_destruction_warlock += 1,
            268 => self.set_tank(TankSpecialization::BrewmasterMonk),
            269 => self.num_windwalker_monk += 1,
            270 => self.set_healer(HealerSpecialization::MistweaverMonk),
            577 => self.num_havoc_dh += 1,
            581 => self.set_tank(TankSpecialization::VengeanceDh),
            _ => {
                warn!("Unspecified specialization ID {}", id);
            }
        }
    }
}

// structures we expect to receive from server
// data is sent in json so using serde_json to parse
mod json {
    #[derive(Deserialize, Debug)]
    pub struct AccessToken {
        pub access_token: String,
        pub token_type: String,
        pub expires_in: u32,
    }

    #[derive(Deserialize, Debug)]
    pub struct LeaderboardIndexEntry {
        pub name: String,
        pub id: u32,
    }

    #[derive(Deserialize, Debug)]
    pub struct Href {
        pub href: String,
    }

    #[derive(Deserialize, Debug)]
    pub struct ConnectedRealmIndex {
        pub connected_realms: Vec<Href>,
    }

    #[derive(Deserialize, Debug)]
    pub struct LeaderboardIndex {
        pub current_leaderboards: Vec<LeaderboardIndexEntry>,
    }

    #[derive(Deserialize, Debug)]
    pub struct FactionType {
        #[serde(rename = "type")]
        pub _type: String,
    }

    #[derive(Deserialize, Debug)]
    pub struct Specialization {
        pub id: u32,
    }

    #[derive(Deserialize, Debug)]
    pub struct Member {
        pub faction: FactionType,
        pub specialization: Specialization,
    }

    #[derive(Deserialize, Debug)]
    pub struct LeadingGroup {
        pub ranking: u32,
        pub duration: u32,
        pub keystone_level: u32,
        pub completed_timestamp: u64,
        pub members: Vec<Member>,
    }

    #[derive(Deserialize, Debug)]
    pub struct DungeonMap {
        pub name: String,
        pub id: u32,
    }

    #[derive(Deserialize, Debug)]
    pub struct KeystoneAffix {
        pub name: String,
    }

    #[derive(Deserialize, Debug)]
    pub struct KeystoneAffixInfo {
        pub keystone_affix: KeystoneAffix,
        pub starting_level: u32,
    }

    #[derive(Deserialize, Debug)]
    pub struct Leaderboard {
        pub map: DungeonMap,
        pub leading_groups: Option<Vec<LeadingGroup>>,
        pub keystone_affixes: Vec<KeystoneAffixInfo>,
    }

    #[derive(Deserialize, Debug)]
    pub struct PeriodId {
        pub id: u32
    }

    #[derive(Deserialize, Debug)]
    pub struct PeriodIndex {
        pub current_period: PeriodId,
        pub periods: Vec<PeriodId>,
    }

    #[derive(Deserialize, Debug)]
    pub struct Period {
        pub id: u32,
        pub start_timestamp: u64,
        pub end_timestamp: u64,
    }
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

#[derive(Debug, Clone)]
struct TimeoutError;

impl std::fmt::Display for TimeoutError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Timeout error")
    }
}

impl error::Error for TimeoutError {
    fn description(&self) -> &str { "Timeout" }
    fn cause(&self) -> Option<&dyn error::Error> { None }
}

fn query<T>(ctx: &Ctx, query: &str) -> Result<T>
where
    T: serde::de::DeserializeOwned,
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

        match serde_json::from_slice(data.as_slice()) {
            Ok(json_value) => {
                if i > 1 {
                    info!("Query \"{}\" that previously timed out is OK", query_str);
                }

                return Ok(json_value);
            }
            Err(err) => {
                error!("Failed to parse json string: {}", String::from_utf8(data)?);
                return Err(Box::new(err));
            }
        }
    }

    Err(Box::new(TimeoutError))
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
            Err(Box::new(err))
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
        result.push(LeaderboardEntry {
            dungeon_name: Dungeon::from_str(&entry.name)?,
            dungeon_id: entry.id,
        });
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

#[derive(StructOpt)]
struct DownloadCmd {
    #[structopt(long, default_value = "eu", help="Region to download from. Either eu, us, tw, kr or cn.")]
    region: Region,

    #[structopt(long, default_value = "10", help="Number of worker threads to spawn.")]
    workers: usize,

    #[structopt(long, default_value = "10", help="Request rate limit (per second).")]
    rate: f32,

    #[structopt(long, help="Time period ID. Use latest time period as default.")]
    period: Option<u32>,

    #[structopt(short = "o", long, help="Output CSV file.", parse(from_os_str))]
    output: Option<path::PathBuf>,

    #[structopt(help="Period index CSV file.", default_value = "data/static/period-index.csv", parse(from_os_str))]
    period_index_file: path::PathBuf,
}

#[derive(StructOpt)]
#[structopt(
    name = "dungeon-crawler",
    about = "Download World of Warcraft mythic+ leaderboard data",
    version = "0.2.0",
    author = "Jaak Randmets <jaak.ra@gmail.com>",
    rename_all = "kebab-case")]
enum Cfg {
    #[structopt(name = "download", about = "Download leaderboard data for a given week")]
    Download(DownloadCmd),

    #[structopt(name = "dedup", about = "Deduplicate a CSV file rows")]
    Dedup {
        #[structopt(help="CSV files to deduplicate.", parse(from_os_str))]
        paths: Vec<path::PathBuf>,
    },
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
        if filter.test_and_add(line_u8) {
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

fn run_dedup(paths: Vec<path::PathBuf>) -> Result<()> {
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

// Configure curl to accept compression and use HTTP2 .
fn configure_easy(easy: &mut Easy) -> Result<()> {
    easy.accept_encoding("gzip, deflate")?;
    easy.pipewait(true)?;
    Ok(())
}

fn run_download(cmd: DownloadCmd) -> Result<()> {

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
                let dungeon = Dungeon::from_str(&leaderboard.map.name)?;
                if let Some(leading_groups) = leaderboard.leading_groups {
                    for leading_group in leading_groups {
                        rows_s.send(DataRow::new(region, dungeon, &leading_group))?;
                    }
                }

                Ok(())
            };

            let queries_r = queries_r.clone();
            let guard = thread::spawn(move || -> Result<()> {
                while let Ok(q) = queries_r.recv() {
                    handle_query(q)?;
                }

                Ok(())
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
                            Err("Unexpected disconnect of a send-channel (queries_s).")?,
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

fn run() -> Result<()> {
    std::env::set_var("RUST_BACˇKTRACE", "1");
    std::env::set_var("RUST_LOG", "dungeon_crawler");
    env_logger::init();
    dotenv().ok();

    match Cfg::from_args() {
        Cfg::Dedup{paths} => run_dedup(paths)?,
        Cfg::Download(cmd) => run_download(cmd)?,
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        println!("{}", err);
        process::exit(1);
    }
}