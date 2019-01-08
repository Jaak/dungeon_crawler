require(data.table)
require(ggplot2)
require(viridis)
require(scales)

# Some general information about a dungeon
# Contains:
# 1) dungeon name in data
# 2) shorthand names and
# 3) completion time limit (in minutes)
DungeonInfo = data.table(
    Dungeon = factor(c("AtalDazar", "Freehold", "TolDagor", "TheMotherlode", "WaycrestManor", "KingsRest", "TempleOfSethraliss", "TheUnderrot", "ShrineOfTheStorms", "SiegeOfBoralus")),
    Shorthand = factor(c("AD", "FH", "TD", "ML", "WM", "KR", "ToS", "UR", "SotS", "SoB")),
    TimeLimit = c(30, 36, 36, 39, 39, 39, 36, 33, 39, 36)
)

ClassInfo = data.table(
    Class = c("DeathKnight", "DemonHunter", "Druid", "Hunter", "Mage", "Monk", "Paladin", "Priest", "Rogue", "Shaman", "Warlock", "Warrior"),
    Color = c("#C41F3B", "#A330C9", "#FF7D0A", "#ABD473", "#40C7EB", "#00FF96", "#F58CBA", "#FFFFFF", "#FFF569", "#0070DE", "#8787ED", "#C79C6E")
)

SpecInfo = data.table(
    Spec = c("BloodDk", "FrostDk", "UnholyDk", "HavocDh", "VengeanceDh", "BalanceDruid", "FeralDruid", "GuardianDruid", "RestorationDruid", "BeastMasteryHunter", "MarksmanshipHunter", "SurvivalHunter", "ArcaneMage", "FireMage", "FrostMage", "BrewmasterMonk", "MistweaverMonk", "WindwalkerMonk", "HolyPaladin", "ProtectionPaladin", "RetributionPaladin", "DisciplinePriest", "HolyPriest", "ShadowPriest", "AssassinationRogue", "OutlawRogue", "SubtletyRogue", "ElementalShaman", "EnhancementShaman", "RestorationShaman", "AfflictionWarlock", "DemonologyWarlock", "DestructionWarlock", "ArmsWarrior", "FuryWarrior", "ProtectionWarrior"),
    Class = c("DeathKnight", "DeathKnight", "DeathKnight", "DemonHunter", "DemonHunter", "Druid", "Druid", "Druid", "Druid", "Hunter", "Hunter", "Hunter", "Mage", "Mage", "Mage", "Monk", "Monk", "Monk", "Paladin", "Paladin", "Paladin", "Priest", "Priest", "Priest", "Rogue", "Rogue", "Rogue", "Shaman", "Shaman", "Shaman", "Warlock", "Warlock", "Warlock", "Warrior", "Warrior", "Warrior"),
    Role = c("Tank", "Melee", "Melee", "Melee", "Tank", "Ranged", "Melee", "Tank", "Healer", "Ranged", "Ranged", "Melee", "Ranged", "Ranged", "Ranged", "Tank", "Healer", "Melee", "Healer", "Tank", "Melee", "Healer", "Healer", "Ranged", "Melee", "Melee", "Melee", "Ranged", "Melee", "Healer", "Ranged", "Ranged", "Ranged", "Melee", "Melee", "Tank")
)

# For each spec the color of choice
SpecColorTable <- SpecInfo[ClassInfo, on = 'Class'][,.(Spec, Role, Color)]
TankColorTable <- SpecColorTable[Role == "Tank"]
TankColors <- TankColorTable$Color
names(TankColors) <- TankColorTable$Spec

AffixCombinations = data.table(
    Id = c(1,2,3,4,5,6,7,8,9,10,11,12),
    Affix1 = factor(rep(c("Fortified", "Tyrannical"), times = 6))
)

# print(AffixCombinations)
# exit(1)

SavePlotAsPng <- function(name, plot) {
    ggsave(name, plot = plot, device = png(), height = 4)
}

# Using column classes for more efficient representation and faster reading.
# If this is slow for you can skip columns by marking the respective class as "NA".
ReadCSVFile <- function(fname) {

    colClasses <-
        c("factor","factor","factor", # Region;Faction;Dungeon;
          "integer64","integer","integer", # Timestamp;Duration;KeystoneLevel
          "factor","factor") # Tank;Healer

    # NumFrostDk;NumUnholyDk;NumHavocDh;NumBalanceDruid;NumFeralDruid;NumBeastMasterHunter
    # NumMarksmanshipHunter;NumSurvivalHunter;NumArcaneMage;NumFireMage;NumFrostMage;NumWindwalkerMonk
    # NumRetributionPaladin;NumShadowPriest;NumAssassinationRogue;NumOutlawRogue;NumSubtletyRogue;NumElementalShaman
    # NumEnhancementShaman;NumAfflictionWarlock;NumDemonologyWarlock;NumDestructionWarlock;NumArmsWarrior;HasFuryWarrior
    colClasses <- c(colClasses, rep("integer", times = 4*6))
    result <- fread(fname, head=TRUE, sep=";", colClasses=colClasses)
    return(result)
}

RunDurationBoxplot <- function(Board) {
    Tbl <- Board[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    Tbl <- Tbl[KeystoneLevel >= 10]
    plot <- ggplot (Tbl, aes(x = reorder(Dungeon, TimeMinutes, mean), y = TimeMinutes, color = Dungeon)) +
        stat_boxplot(geom ='errorbar', lwd=0.25) +
        geom_boxplot(lwd=0.25, outlier.size = 0.1) +
        geom_point(aes(x=Shorthand, y=TimeLimit, color=Shorthand), data = DungeonInfo, size = 2) +
        scale_y_log10() +
        xlab("Dungeon") +
        ylab("Time (in minutes)") +
        ggtitle ("Median run length of each dungeon (compared to time limit)")

    return (plot)
}

NumberOfRuns <- function(frame) {
    summary <- frame[, list(Runs = .N), keyby=.(KeystoneLevel,Dungeon)]
    plot <- ggplot(data=summary, aes(x = factor(KeystoneLevel), y = Runs, fill = Dungeon)) +
        geom_bar(stat="identity") +
        xlab("Keystone level") +
        ggtitle("Number of runs per keystone level")

    return (plot)
}

SuccessRateBaseOnKeyLevel <- function(Board) {
    summary <- Board[, list(SuccessRate = sum(Success) / .N), keyby=.(KeystoneLevel, Dungeon)]
    plot <- ggplot(data=summary, aes(x = factor(KeystoneLevel), y = SuccessRate, fill=Dungeon)) +
        geom_bar(stat="identity") +
        xlab("Keystone level") +
        ggtitle("Success rate of each dungeon per keystone level")

    return (plot)
}

TankPrecentage <- function(Board) {
    TankRuns <- Board[Tank != "",.(Count=.N), keyby=.(KeystoneLevel,Tank)]
    TankRuns[,Percent:=sum(Count),by=KeystoneLevel]
    TankRuns[,Percent:=100*Count/Percent]
    TankRuns <- TankRuns[Count > 1]
    KeyRange <- 2:max(TankRuns$KeystoneLevel)
    plot <- ggplot(data=TankRuns, aes(x=KeystoneLevel, y=Percent, color=Tank)) +
        scale_color_manual(values = TankColors) +
        geom_line() +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        xlab("Keystone level") +
        ggtitle("Tanks by keystone level")
    return (plot)
}

HealerPrecentage <- function(Board) {
    HealerRuns <- Board[Healer != "",.(Count=.N), keyby=.(KeystoneLevel,Healer)]
    HealerRuns[,Percent:=sum(Count),by=KeystoneLevel]
    HealerRuns[,Percent:=100*Count/Percent]
    HealerRuns <- HealerRuns[Count > 1]
    KeyRange <- 2:max(HealerRuns$KeystoneLevel)
    plot <- ggplot(data=HealerRuns, aes(x=KeystoneLevel, y=Percent, color=Healer)) +
        geom_line() +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        xlab("Keystone level") +
        ggtitle("Healers by keystone level")
    return (plot)
}

MeleeCountPrecentage <- function(Board) {
    MeleeRuns <- Board[
        ,
        .(Count = sum(Success), Percent=100.0 * sum(Success) / .N),
        keyby=.(KeystoneLevel,NumMelee)]
    MeleeRuns <- MeleeRuns[Count > 1]
    KeyRange <- 2:max(MeleeRuns$KeystoneLevel)
    MeleeRuns[, NumMelee:=factor(NumMelee)]
    plot <- ggplot(data=MeleeRuns, aes(x=KeystoneLevel, y=Percent, color=NumMelee)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        xlab("Keystone level") +
        ggtitle("Success rate by number of melee")
    return (plot)
}

TankSuccessByKey <- function(Board) {
    TotalChances <- Board[, .(AvgChance = sum(Success) / .N), keyby=.(KeystoneLevel)]
    Tbl <- Board[
        Tank != "",
        .(Chance = sum(Success) / .N),
        keyby=.(KeystoneLevel,Tank)]
    Tbl <- Tbl[TotalChances, on = 'KeystoneLevel', Ratio := Chance / AvgChance]
    plot <- ggplot(data=Tbl, aes(x=factor(KeystoneLevel), y=Ratio, color=Tank)) +
        geom_line() +
        xlab("Keystone level") +
        ggtitle("Success rate by tank")
    return (plot)
}

DiversitySuccessRate <- function(Board) {
    Runs <- Board[,.(Count=.N), keyby=.(KeystoneLevel,Success,Diversity)]
    Runs[,TotalForKey:=sum(Count),by=.(KeystoneLevel,Diversity)]
    Runs[,Percent:=100*Count/TotalForKey]
    Runs <- Runs[(Success)]
    Runs[,Diversity:=factor(Diversity)]
    plot <- ggplot(data=Runs, aes(x=factor(KeystoneLevel), y=Percent, color=Diversity)) +
        geom_line() +
        xlab("Keystone level") +
        ggtitle("Success rate by number of different realms")
    return (plot)
}

SuccessRateByDungeon <- function(Board, L) {
    DungeonRuns <- Board[KeystoneLevel==L,.(Y=sum(Success), N=.N-sum(Success)), keyby=.(Dungeon)]
    DungeonRuns <- melt(DungeonRuns, id.vars=c("Dungeon"), variable.name="Success", value.name="Count")
    DungeonRuns[,Success:=Success=="Y"]
    DungeonRuns <- DungeonRuns[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(DungeonRuns, aes(fill=Success, y=Count, x=reorder(Dungeon,-Count))) +
        geom_bar(stat="identity") +
        ggtitle(paste("+", L, " Success rate by dungeon", sep = "")) +
        xlab("Dungeon")
    return (plot)
}

HealerHeatmap <- function(Board, L) {
    Tbl <- Board[KeystoneLevel==L & Healer!="",
                 .(SuccessRate=sum(Success) / .N),
                 keyby=.(Dungeon, Healer)]
    Tbl <- Tbl[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Tbl, aes(x=reorder(Dungeon, -SuccessRate, mean),
                                 y=reorder(Healer, SuccessRate, mean))) +
        scale_fill_viridis(option = "cividis") +
        geom_tile(aes(fill = SuccessRate), colour = "white") +
        geom_text(aes(label = round(SuccessRate, 1)), size=2) +
        coord_equal() +
        xlab("Dungeon") +
        ylab("Healer") +
        ggtitle(paste("Healer success rate in each dungeon for +", L, " keystone", sep=""))
    return (plot)
}

TankHeatmap <- function(Board, L) {
    Tbl <- Board[KeystoneLevel==L & Tank!=""]
    Tbl <- Tbl[,.(SuccessRate=sum(Success) / .N), keyby=.(Dungeon, Tank)]
    Tbl <- Tbl[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Tbl, aes(x=reorder(Dungeon, -SuccessRate, mean),
                                 y=reorder(Tank, SuccessRate, mean))) +
        scale_fill_viridis(option = "cividis") +
        geom_tile(aes(fill = SuccessRate), colour = "white") +
        geom_text(aes(label = round(SuccessRate, 1)), size=2) +
        theme_bw() +
        coord_equal() +
        xlab("Dungeon") +
        ylab("Tank") +
        ggtitle(paste("Tank success rate in each dungeon for +", L, " keystone", sep=""))
    return (plot)
}

KeystoneLevelHeatmap <- function(Board) {
    Tbl <- Board[,
                 .(SuccessRate=sum(Success) / .N),
                 keyby=.(Dungeon, KeystoneLevel)]
    KeyRange <- 2:max(Tbl$KeystoneLevel)
    Tbl <- Tbl[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Tbl, aes(x=factor(KeystoneLevel),
                                 y=reorder(Dungeon, SuccessRate, mean))) +
        geom_tile(aes(fill = SuccessRate), colour = "white") +
        scale_fill_viridis(option = "cividis") +
        geom_text(aes(label = round(SuccessRate, 1)), size=2) +
        scale_x_discrete(breaks=KeyRange, labels=factor(KeyRange)) +
        coord_equal() +
        xlab("Keystone level") +
        ylab("Dungeon") +
        ggtitle("Success rate for each dungeon and keystone level combination")
    return (plot)
}

RunStartTimeDistribution <- function(Board) {
    Tbl <- Board[,.(Hour=as.POSIXct(round(Datetime, "hour")))]
    Tbl <- Tbl[, .(Count = .N), keyby=.(Hour)]
    plot <- ggplot(data=Tbl, aes(x=Hour, y=Count)) +
        geom_bar(stat = "identity") +
        scale_x_datetime(breaks = date_breaks("1 day"))
    return (plot)
}

args <- commandArgs(TRUE)

# Read the leaderboard data
Board <- ReadCSVFile(args[1])

# Has to be better way to only get uniques and attach group size
Board[, Diversity := .N, keyby=.(Timestamp,Duration,Dungeon,KeystoneLevel,Tank,Healer)]
Board <- unique(Board)

# Count number of melee
Board[,NumMelee:=NumFrostDk+NumUnholyDk+NumHavocDh+NumFeralDruid+NumSurvivalHunter+NumWindwalkerMonk+NumRetributionPaladin+NumAssassinationRogue+NumOutlawRogue+NumSubtletyRogue+NumEnhancementShaman+NumArmsWarrior+NumFuryWarrior]

# More readable duration
Board[,TimeMinutes:=Duration/60000]

# Attach information if run was successfully on time or not
Board <- Board[DungeonInfo, on='Dungeon', Success:=TimeMinutes<=TimeLimit]

Board[, Datetime:=as.POSIXct(Timestamp/1000, origin="1970-01-01")]

#
# General style
#

theme_set(theme_bw())  # pre-set the bw theme.

#
# Bunch of graphs:
#

if (FALSE) {
    SavePlotAsPng("timestamp.png", RunStartTimeDistribution(Board))
}

if (FALSE) {
    SavePlotAsPng("number-of-runs.png", NumberOfRuns(Board))
}

if (FALSE) {
    SavePlotAsPng("tank-precentage.png", TankPrecentage(Board))
}

if (FALSE) {
    SavePlotAsPng("healer-precentage.png", HealerPrecentage(Board))
}

if (FALSE) {
    SavePlotAsPng("healer-heatmap.png", HealerHeatmap(Board, 15))
}

if (FALSE) {
    SavePlotAsPng("tank-heatmap.png", TankHeatmap(Board, 15))
}

if (FALSE) {
    SavePlotAsPng("melee-precentage.png", MeleeCountPrecentage(Board))
}

if (FALSE) {
    SavePlotAsPng("success-by-dungeon-15.png", SuccessRateByDungeon(Board, 15))
}

if (FALSE) {
    SavePlotAsPng("keystone-level-heatmap.png", KeystoneLevelHeatmap(Board))
}

if (FALSE) {
    SavePlotAsPng("duration-boxplot.png", RunDurationBoxplot(Board))
}

#
# A bit useless plots:
#

if (FALSE) {
    SavePlotAsPng("group-diversity-success-rate.png", DiversitySuccessRate(Board))
}

if (FALSE) {
    SavePlotAsPng("tank-success.png", TankSuccessByKey(Board))
}

if (FALSE) {
    SavePlotAsPng("overall-success-rate.png", SuccessRateBaseOnKeyLevel(Board))
}