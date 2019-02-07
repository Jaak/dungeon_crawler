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

type Result<T> = std::result::Result<T, Box<error::Error + Send + Sync>>;

#[allow(dead_code)]
#[derive(Debug, Serialize)]
enum TankSpecialization {
    ProtectionPaladin = 1,
    ProtectionWarrior,
    BloodDk,
    VengeanceDh,
    GuardianDruid,
    BrewmasterMonk,
}

#[allow(dead_code)]
#[derive(Debug, Serialize)]
enum HealerSpecialization {
    RestorationDruid = 1,
    MistweaverMonk,
    HolyPaladin,
    DisciplinePriest,
    HolyPriest,
    RestorationShaman,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Serialize)]
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

#[allow(dead_code)]
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
    type Err = &'static str;

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

        HASH_MAP.get(s).cloned().ok_or("Missing entry")
    }
}

#[allow(dead_code)]
#[derive(Debug, Serialize)]
enum Faction {
    ALLIANCE = 1,
    HORDE,
}

// Make sure that Option<> optimization works correctly with our enums.
assert_eq_size!(enum_size_1; Option<TankSpecialization>, TankSpecialization);
assert_eq_size!(enum_size_2; Option<HealerSpecialization>, HealerSpecialization);
assert_eq_size!(enum_size_3; Option<Region>, Region);
assert_eq_size!(enum_size_4; Option<Dungeon>, Dungeon);
assert_eq_size!(enum_size_5; Option<Faction>, Faction);

#[allow(dead_code)]
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

    fn set_healer(&mut self, spec: HealerSpecialization) -> bool {
        if self.healer.is_some() {
            return false;
        }

        self.healer = Some(spec);
        return true;
    }

    fn set_tank(&mut self, spec: TankSpecialization) -> bool {
        if self.tank.is_some() {
            return false;
        }

        self.tank = Some(spec);
        return true;
    }

    fn update_faction(&mut self, faction_name: &str) {
        match faction_name.to_uppercase().as_str() {
            "ALLIANCE" => { self.faction.replace(Faction::ALLIANCE); },
            "HORDE" => { self.faction.replace(Faction::HORDE); },
            _ => {},
        }
    }

    fn update_group_composition(&mut self, id: u32) -> bool {
        match id {
            62 => { self.num_arcane_mage += 1; true },
            63 => { self.num_fire_mage += 1; true },
            64 => { self.num_frost_mage += 1; true },
            65 => self.set_healer(HealerSpecialization::HolyPaladin),
            66 => self.set_tank(TankSpecialization::ProtectionPaladin),
            70 => { self.num_retribution_paladin += 1; true },
            71 => { self.num_arms_warrior += 1; true },
            72 => { self.num_fury_warrior += 1; true },
            73 => self.set_tank(TankSpecialization::ProtectionWarrior),
            102 => { self.num_balance_druid += 1; true },
            103 => { self.num_feral_druid += 1; true },
            104 => self.set_tank(TankSpecialization::GuardianDruid),
            105 => self.set_healer(HealerSpecialization::RestorationDruid),
            250 => self.set_tank(TankSpecialization::BloodDk),
            251 => { self.num_frost_dk += 1; true },
            252 => { self.num_unholy_dk += 1; true },
            253 => { self.num_beast_master_hunter += 1; true },
            254 => { self.num_marksmanship_hunter += 1; true },
            255 => { self.num_survival_hunter += 1; true },
            256 => self.set_healer(HealerSpecialization::DisciplinePriest),
            257 => self.set_healer(HealerSpecialization::HolyPriest),
            258 => { self.num_shadow_priest += 1; true },
            259 => { self.num_assassination_rogue += 1; true },
            260 => { self.num_outlaw_rogue += 1; true },
            261 => { self.num_subtlety_rogue += 1; true },
            262 => { self.num_elemental_shaman += 1; true },
            263 => { self.num_enhancement_shaman += 1; true },
            264 => self.set_healer(HealerSpecialization::RestorationShaman),
            265 => { self.num_affliction_warlock += 1; true },
            266 => { self.num_demonology_warlock += 1; true },
            267 => { self.num_destruction_warlock += 1; true },
            268 => self.set_tank(TankSpecialization::BrewmasterMonk),
            269 => { self.num_windwalker_monk += 1; true },
            270 => self.set_healer(HealerSpecialization::MistweaverMonk),
            577 => { self.num_havoc_dh += 1; true },
            581 => self.set_tank(TankSpecialization::VengeanceDh),
            _ => {
                warn!("Unspecified specialization ID {}", id);
                false
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
    pub struct Href {
        pub href: String,
    }

    #[derive(Deserialize, Debug)]
    pub struct _Self {
        #[serde(rename = "self")]
        pub _self: Href,
    }

    #[derive(Deserialize, Debug)]
    pub struct LeaderboardIndexEntry {
        pub key: Href,
        pub name: String,
        pub id: u32,
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

    let data = write_to_vector(&mut easy)?;
    match serde_json::from_slice(data.as_slice()) {
        Ok(json_value) => Ok(json_value),
        Err(err) => {
            let raw_json: serde_json::Value = serde_json::from_slice(data.as_slice())?;
            error!("Failed to parse json string: {}", raw_json);
            Err(Box::new(err))
        }
    }
}

// Resulting token is just printed out to stdout.
fn token_request(
    easy: &mut Easy,
    region: Region,
    client_id: &str,
    client_secret: &str,
) -> Result<json::AccessToken> {
    let url = &format!(
        "{uri}?grant_type=client_credentials&client_id={client_id}&client_secret={client_secret}",
        uri=region.token_uri(),
        client_id=client_id,
        client_secret=client_secret
    );

    easy.url(url).unwrap();

    let mut list = List::new();
    list.append("Accept: application/json").unwrap();
    easy.http_headers(list).unwrap();

    let data = write_to_vector(easy)?;
    let json_value = serde_json::from_slice(data.as_slice())?;
    Ok(json_value)
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
    period: u32,
}

fn query_mythic_leaderboard_index(ctx: &Ctx, realm_id: u32) -> Result<Vec<LeaderboardEntry>> {
    lazy_static! {
        static ref MATCH_URL: Regex =
            Regex::new(r"/mythic-leaderboard/(?P<dungeonId>\d+)/period/(?P<period>\d+)").unwrap();
    }

    let query_str = &format!(
        "data/wow/connected-realm/{connectedRealmId}/mythic-leaderboard/index",
        connectedRealmId = realm_id);
    let leaderboard: json::LeaderboardIndex = query(ctx, query_str)?;
    let mut result = Vec::new();
    for entry in leaderboard.current_leaderboards {
        let url = entry.key.href;
        let dungeon_name = Dungeon::from_str(&entry.name)?;
        let dungeon_id = entry.id;
        for c in MATCH_URL.captures_iter(&url) {
            let dungeon_id2 = c.name("dungeonId").unwrap().as_str().parse::<u32>()?;
            assert_eq!(dungeon_id, dungeon_id2);
            let period = c.name("period").unwrap().as_str().parse()?;
            result.push(LeaderboardEntry {
                dungeon_name,
                dungeon_id,
                period,
            });
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
    #[structopt(long, help="Print information about latest period ID.")]
    show_latest_period: bool,
}

#[derive(StructOpt)]
#[structopt(
    name = "dungeon-crawler",
    about = "Download World of Warcraft mythic+ leaderboard data",
    version = "0.2.0",
    author = "Jaak Randmets <jaak.ra@gmail.com>",
    rename_all = "kebab-case")]
enum Cfg {
    #[structopt(name = "download")]
    Download(DownloadCmd),
    #[structopt(name = "dedup")]
    Dedup {
        #[structopt(help="CSV files to deduplicate.", parse(from_os_str))]
        paths: Vec<path::PathBuf>,
    }
}

// Deduplicate rows (possibly imperfectly) of a given text file.
// The file in given path will be overwritten by deduplicated file.
// Note that with a low probability the result may still have some
// duplicate rows.
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

fn run_download(cmd: DownloadCmd) -> Result<()> {
    let client_id = &env::var("CLIENT_ID")?;
    let client_secret = &env::var("CLIENT_SECRET")?;

    info!("Requesting token...");
    let mut easy = Easy::new();
    let DownloadCmd{region, workers, rate, period, output, show_latest_period} = cmd;
    let access_token = &token_request(&mut easy, region, client_id, client_secret)?;

    // Global context
    let gctx = &Ctx {
        access_token: access_token.access_token.clone(),
        region: region,
        easy: RefCell::new(easy),
    };

    let period_id = match period {
        None => query_period_index(gctx)?.current_period.id,
        Some(id) => id,
    };

    // TODO: weirdness with this option
    if show_latest_period {
        let vec = query_period_index(gctx)?.periods;
        if let Some(index) = vec.last() {
            let period = query_period(gctx, index.id)?;
            println!("{:?};{};{};{}", region, index.id, period.start_timestamp, period.end_timestamp);
        }

        process::exit(0);
    }

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
            easy.accept_encoding("gzip")?;

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
                    recv(ticker) -> _ => { wrt.flush()?; },
                    recv(rows_r) -> row => match row {
                        Ok(row) => { wrt.serialize(row)?; },
                        Err(_) => { break; },
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
    env_logger::init();
    dotenv().ok();

    match Cfg::from_args() {
        Cfg::Dedup{paths} => { run_dedup(paths)?; },
        Cfg::Download(cmd) => { run_download(cmd)?; },
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        println!("{}", err);
        process::exit(1);
    }
}