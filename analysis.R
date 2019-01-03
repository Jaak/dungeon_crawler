require(data.table)
require(ggplot2)
# require(scales)

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

RunDurationDensityPlot <- function(frame) {
    plot <- ggplot (frame[Duration < 100*60*1000], aes(x = Duration / (1000.0 * 60.0), color = Dungeon)) +
        geom_density() +
        theme_bw() +
        ggtitle ("Distribution of run length")

    return (plot)
}

NumberOfRuns <- function(frame) {
    summary <- frame[, list(Runs = .N), keyby=.(KeystoneLevel,Dungeon)]
    plot <- ggplot(data=summary, aes(x = KeystoneLevel, y = Runs, fill = Dungeon)) +
        geom_bar(stat="identity") +
        theme_bw() +
        ggtitle("Number of runs per keystone level")

    return (plot)
}

SuccessRateBaseOnKeyLevel <- function(Board) {
    summary <- Board[, list(SuccessRate = sum(Success) / .N), keyby=.(KeystoneLevel, Dungeon)]
    plot <- ggplot(data=summary, aes(x = KeystoneLevel, y = SuccessRate, fill=Dungeon)) +
        geom_bar(stat="identity") +
        theme_bw() +
        ggtitle("Success rate of each dungeon per keystone level")

    return (plot)
}

TankPrecentage <- function(Board) {
    TankRuns <- Board[,.(Count=.N), keyby=.(KeystoneLevel,Tank)]
    TankRuns <- TankRuns[Tank != ""]
    TankRuns[,Percent:=sum(Count),by=KeystoneLevel]
    TankRuns[,Percent:=100*Count/Percent]
    plot <- ggplot(data=TankRuns, aes(x=KeystoneLevel, y=Percent, color=Tank)) +
        geom_line() +
        theme_bw() +
        ggtitle("Tanks by keystone level")
    return (plot)
}

HealerPrecentage <- function(Board) {
    HealerRuns <- Board[,.(Count=.N), keyby=.(KeystoneLevel,Healer)]
    HealerRuns[,Percent:=sum(Count),by=KeystoneLevel]
    HealerRuns[,Percent:=100*Count/Percent]
    plot <- ggplot(data=HealerRuns, aes(x=KeystoneLevel, y=Percent, color=Healer)) +
        geom_line() +
        theme_bw() +
        ggtitle("Healers by keystone level")
    return (plot)
}

MeleeCountPrecentage <- function(Board) {
    MeleeRuns <- Board[,.(Count=.N), keyby=.(KeystoneLevel,Success,NumMelee)]
    MeleeRuns[,TotalForKey:=sum(Count),by=.(KeystoneLevel,NumMelee)]
    MeleeRuns[,Percent:=100*Count/TotalForKey]
    MeleeRuns <- MeleeRuns[(Success)]
    MeleeRuns[,NumMelee:=factor(NumMelee)]
    plot <- ggplot(data=MeleeRuns, aes(x=KeystoneLevel, y=Percent, color=NumMelee)) +
        geom_line() +
        theme_bw() +
        ggtitle("Success rate by number of melee")
    return (plot)
}

SuccessRateByDungeon <- function(Board, L) {
    DungeonRuns <- Board[KeystoneLevel==L,.(Y=sum(Success), N=.N-sum(Success)), keyby=.(Dungeon)]
    DungeonRuns <- melt(DungeonRuns, id.vars=c("Dungeon"), variable.name="Success", value.name="Count")
    DungeonRuns[,Success:=Success=="Y"]
    DungeonRuns <- DungeonRuns[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(DungeonRuns, aes(fill=Success, y=Count, x=reorder(Dungeon,-Count))) +
        geom_bar(stat="identity") +
        theme_bw() +
        ggtitle(paste("+", L, " Success rate by dungeon", sep = "")) +
        xlab("Dungeon")
    return (plot)
}

args <- commandArgs(TRUE)

# Read the leaderboard data
Board <- ReadCSVFile(args[1])

# Count number of melee
Board[,NumMelee:=NumFrostDk+NumUnholyDk+NumHavocDh+NumFeralDruid+NumSurvivalHunter+NumWindwalkerMonk+NumRetributionPaladin+NumAssassinationRogue+NumOutlawRogue+NumSubtletyRogue+NumEnhancementShaman+NumArmsWarrior+NumFuryWarrior]

# More readable duration
Board[,TimeMinutes:=Duration/60000]

# Attach information if run was successfully on time or not
Board <- Board[DungeonInfo, on='Dungeon', Success:=TimeMinutes<=TimeLimit]

# Plot crapton of stuff

if (FALSE) {
    SavePlotAsPng("number-of-runs.png", NumberOfRuns(Board))
}

if (FALSE) {
    SavePlotAsPng("overall-success-rate.png", SuccessRateBaseOnKeyLevel(Board))
}

if (FALSE) {
    SavePlotAsPng("tank-precentage.png", TankPrecentage(Board))
}

if (FALSE) {
    SavePlotAsPng("healer-precentage.png", HealerPrecentage(Board))
}

if (FALSE) {
    SavePlotAsPng("duration-density.png", RunDurationDensityPlot(Board))
}

if (FALSE) {
    SavePlotAsPng("melee-precentage.png", MeleeCountPrecentage(Board))
}

if (FALSE) {
    SavePlotAsPng("success-by-dungeon-15.png", SuccessRateByDungeon(Board, 15))
}