// Data structures we expect to receive from BNet leaderboard API.
// Data is sent in json.
// We are using serde_json to parse.

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