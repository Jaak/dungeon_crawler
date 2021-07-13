use crate::json;
use std::str::FromStr;
use log::warn;

#[derive(Debug, Serialize)]
pub enum TankSpecialization {
    ProtectionPaladin = 1,
    ProtectionWarrior,
    BloodDk,
    VengeanceDh,
    GuardianDruid,
    BrewmasterMonk,
}

#[derive(Debug, Serialize)]
pub enum HealerSpecialization {
    RestorationDruid = 1,
    MistweaverMonk,
    HolyPaladin,
    DisciplinePriest,
    HolyPriest,
    RestorationShaman,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum Region {
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

#[derive(Debug, Serialize)]
pub enum Faction {
    ALLIANCE = 1,
    HORDE,
}

// Make sure that Option<> optimization works correctly with our enums.
assert_eq_size!(Option<TankSpecialization>, TankSpecialization);
assert_eq_size!(Option<HealerSpecialization>, HealerSpecialization);
assert_eq_size!(Option<Region>, Region);
assert_eq_size!(Option<Faction>, Faction);

#[serde(rename_all = "PascalCase")]
#[derive(Default, Debug, Serialize)]
pub struct DataRow {
    region: Option<Region>,
    faction: Option<Faction>,
    dungeon: u32,
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
    pub fn new(region: Region, dungeon: u32, group: &json::LeadingGroup) -> DataRow {
        let mut result = DataRow {
            region: Some(region),
            faction: None,
            dungeon,
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

/*
#[serde(rename_all = "PascalCase")]
#[derive(Default, Debug, Serialize)]
pub struct DataRowV2 {
    region: Region,
    faction: Faction,
    dungeon: Dungeon,
    timestamp: u64,
    duration: u32,
    keystone_level: u32,
    period_id: u32,
    success: bool,

    // Tank:
    num_protection_paladin: u8,
    num_protection_warrior: u8,
    num_blood_dk: u8,
    num_vengeance_dh: u8,
    num_guardian_druid: u8,
    num_brewmaster_monk: u8,
    // Healer:
    num_restoration_druid: u8,
    num_mistweaver_monk: u8,
    num_holy_paladin: u8,
    num_discipline_priest: u8,
    num_holy_priest: u8,
    num_restoration_shaman: u8,
    // Dps:
    #[serde(flatten)]
    dps: DpsComposition,
}

impl DataRowV2 {
    pub fn new(region: Region, dungeon: Dungeon, group: &json::LeadingGroup) -> Option<DataRow> {
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
            if ! result.update_faction(&json_member.faction._type) {
                warn!("Messed up faction of a group.");
                return None();
            }
        }

        if result.faction.is_none() {
            return None();
        }

        Some(result)
    }

    fn update_faction(&mut self, faction_name: &str) -> bool {
        match faction_name.to_uppercase().as_str() {
            "ALLIANCE" => {
                if self.faction == Some(Faction::HORDE) {
                    return false;
                }

                self.faction.replace(Faction::ALLIANCE); },
            "HORDE" => {
                if self.faction == Some(Faction::ALLIANCE) {
                    return false;
                }

                self.faction.replace(Faction::HORDE);
            },
            _ => { return false; },
        }

        return true;
    }

    fn update_group_composition(&mut self, id: u32) {
        match id {
            62 => self.dps.num_arcane_mage += 1,
            63 => self.dps.num_fire_mage += 1,
            64 => self.dps.num_frost_mage += 1,
            65 => self.num_holy_paladin += 1,
            66 => self.num_protection_paladin += 1,
            70 => self.dps.num_retribution_paladin += 1,
            71 => self.dps.num_arms_warrior += 1,
            72 => self.dps.num_fury_warrior += 1,
            73 => self.num_protection_warrior += 1,
            102 => self.dps.num_balance_druid += 1,
            103 => self.dps.num_feral_druid += 1,
            104 => self.num_guardian_druid += 1,
            105 => self.num_restoration_druid += 1,
            250 => self.num_blood_dk += 1,
            251 => self.dps.num_frost_dk += 1,
            252 => self.dps.num_unholy_dk += 1,
            253 => self.dps.num_beast_master_hunter += 1,
            254 => self.dps.num_marksmanship_hunter += 1,
            255 => self.dps.num_survival_hunter += 1,
            256 => self.num_discipline_priest += 1,
            257 => self.num_holy_priest += 1,
            258 => self.dps.num_shadow_priest += 1,
            259 => self.dps.num_assassination_rogue += 1,
            260 => self.dps.num_outlaw_rogue += 1,
            261 => self.dps.num_subtlety_rogue += 1,
            262 => self.dps.num_elemental_shaman += 1,
            263 => self.dps.num_enhancement_shaman += 1,
            264 => self.num_restoration_shaman += 1,
            265 => self.dps.num_affliction_warlock += 1,
            266 => self.dps.num_demonology_warlock += 1,
            267 => self.dps.num_destruction_warlock += 1,
            268 => self.num_brewmaster_monk += 1,
            269 => self.dps.num_windwalker_monk += 1,
            270 => self.num_mistweaver_monk += 1,
            577 => self.dps.num_havoc_dh += 1,
            581 => self.num_vengeance_dh += 1,
            _ => {
                warn!("Unspecified specialization ID {}", id);
            }
        }
    }
}
*/