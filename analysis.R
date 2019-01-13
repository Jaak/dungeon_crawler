library(data.table)
library(ggplot2)
library(viridis)
library(scales)

ReadSpecInfo <- function() {
    colClasses <- c("factor","factor","factor")
    result <- fread("data/static/spec-info.csv",
                    head=TRUE, sep=";", colClasses=colClasses, key=c("Spec"))
    return(result)
}

ReadClassInfo <- function() {
    colClasses <- c("factor","factor","character")
    result <- fread("data/static/class-info.csv",
                    head=TRUE, sep=";", colClasses=colClasses, key=c("Class"))
    return(result)
}

ReadDungeonInfo <- function() {
    colClasses <- c("factor","factor","integer")
    result <- fread("data/static/dungeon-info.csv",
                    head=TRUE, sep=";", colClasses=colClasses, key=c("Dungeon"))
    return(result)
}

ReadAffixInfo <- function() {
    colClasses <- c("integer","factor","factor", "factor", "factor")
    result <- fread("data/static/affix-info.csv",
                    head=TRUE, sep=";", colClasses=colClasses, key=c("PeriodId"))
    return(result)
}

SpecInfo <- ReadSpecInfo()
ClassInfo <- ReadClassInfo()
DungeonInfo <- ReadDungeonInfo()
AffixInfo <- ReadAffixInfo()

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
    plot <- ggplot (Tbl, aes(x = reorder(Dungeon, TimeMinutes, median), y = TimeMinutes, color = Dungeon)) +
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
    plot <- ggplot(data=summary, aes(x = factor(KeystoneLevel), y = Runs)) +
        geom_bar(stat="identity") +
        xlab("Keystone level") +
        ggtitle("Number of runs per keystone level")

    return (plot)
}

DungeonPopularity <- function(Board) {
    Dungeons <- Board[, .(Count = .N), keyby=.(KeystoneLevel,Dungeon)]
    Dungeons <- Dungeons[CJ(KeystoneLevel, Dungeon, unique=TRUE)][is.na(Count), Count := 0]
    plot <- ggplot(data=Dungeons,
            aes(x = KeystoneLevel,
                y = Count,
                fill = reorder(Dungeon, -Count, sum))) +
        geom_area(position="fill") +
        scale_y_continuous(labels = percent) +
        xlab("Keystone level") +
        ggtitle("Popularity of dungeons based on keystone level")

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
    TankRuns[, Percent := sum(Count), by=KeystoneLevel]
    TankRuns[, Percent := Count/Percent]
    KeyRange <- 2:max(TankRuns$KeystoneLevel)
    plot <- ggplot(data=TankRuns,
        aes(x=KeystoneLevel, y=Percent, color=Tank)) +
        scale_color_manual(values = TankColors) +
        geom_line() +
        scale_x_continuous(
            breaks=KeyRange[KeyRange %% 5 == 0],
            minor_breaks=KeyRange) +
        scale_y_continuous(labels = percent) +
        # scale_fill_manual(values = TankColors, name = "Tank") +
        # geom_area(position="fill") +
        xlab("Keystone level") +
        ylab("Percentage of tanks") +
        ggtitle("Tanks by keystone level")
    return (plot)
}

HealerPrecentage <- function(Board) {
    HealerRuns <- Board[Healer != "",.(Count=.N), keyby=.(KeystoneLevel,Healer)]
    HealerRuns[,Percent:=sum(Count),by=KeystoneLevel]
    HealerRuns[,Percent:=Count/Percent]
    KeyRange <- 2:max(HealerRuns$KeystoneLevel)
    plot <- ggplot(data=HealerRuns, aes(x=KeystoneLevel, y=Percent, color=Healer)) +
        geom_line() +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        scale_y_continuous(labels = percent) +
        xlab("Keystone level") +
        ggtitle("Healers by keystone level")
    return (plot)
}

MeleeCountPrecentage <- function(Board) {
    MeleeRuns <- Board[
        ,
        .(Count = sum(Success), Percent=sum(Success) / .N),
        keyby=.(KeystoneLevel,NumMelee)]
    MeleeRuns <- MeleeRuns[Count > 1]
    KeyRange <- 2:max(MeleeRuns$KeystoneLevel)
    MeleeRuns[, NumMelee:=factor(NumMelee)]
    plot <- ggplot(data=MeleeRuns, aes(x=KeystoneLevel, y=Percent, color=NumMelee)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        scale_y_continuous(labels = percent) +
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

SuccessRateByDungeon <- function(Board, L) {
    DungeonRuns <- Board[KeystoneLevel==L,.(Count=.N), keyby=.(Dungeon, Success)]
    # Not needed any more but here to remind myself of how to fill in missing values:
    # DungeonRuns <- DungeonRuns[CJ(Dungeon, Success, unique=TRUE)][is.na(Count), Count := 0]
    DungeonRuns <- DungeonRuns[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(DungeonRuns, aes(y=Count, x=reorder(Dungeon,-Count,sum), fill=Success)) +
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
    Ord <- Board[, .(AvgSuccess = sum(Success) / .N), keyby=.(Dungeon)]
    Tbl <- Board[, .(SuccessRate=sum(Success) / .N), keyby=.(Dungeon, KeystoneLevel)]
    Tbl <- Tbl[Ord, on = 'Dungeon']
    KeyRange <- 2:max(Tbl$KeystoneLevel)
    Tbl <- Tbl[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Tbl, aes(x=factor(KeystoneLevel),
                                 y=reorder(Dungeon, AvgSuccess, first))) +
        geom_tile(aes(fill = SuccessRate), colour = "white") +
        scale_fill_viridis(option = "cividis") +
        geom_text(aes(label = round(SuccessRate, 1)), size=2) +
        scale_x_discrete(breaks=KeyRange, labels=KeyRange) +
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

theme_set(theme_bw())

#
# Plotly test:
#

if (FALSE) {
    library(plotly)
    p <- SuccessRateByDungeonA(Board)
    p <- ggplotly(p)
    htmlwidgets::saveWidget(as_widget(p), "graph.html")
}

#
# Bunch of graphs:
#

if (TRUE) {
    SavePlotAsPng("number-of-runs.png", NumberOfRuns(Board))
}

if (TRUE) {
    SavePlotAsPng("tank-precentage.png", TankPrecentage(Board))
}

if (TRUE) {
    SavePlotAsPng("healer-precentage.png", HealerPrecentage(Board))
}

if (TRUE) {
    SavePlotAsPng("melee-precentage.png", MeleeCountPrecentage(Board))
}

if (TRUE) {
    SavePlotAsPng("success-by-dungeon-15.png", SuccessRateByDungeon(Board, 15))
}

if (TRUE) {
    SavePlotAsPng("keystone-level-heatmap.png", KeystoneLevelHeatmap(Board))
}

if (TRUE) {
    SavePlotAsPng("duration-boxplot.png", RunDurationBoxplot(Board))
}

#
# Occasionally interesting plots:
#

if (TRUE) {
    SavePlotAsPng("healer-heatmap.png", HealerHeatmap(Board, 15))
}

if (TRUE) {
    SavePlotAsPng("tank-heatmap.png", TankHeatmap(Board, 15))
}

#
# A bit boring but still little informative plots:
#

if (FALSE) {
    SavePlotAsPng("dungeon-popularity.png", DungeonPopularity(Board))
}

if (FALSE) {
    SavePlotAsPng("timestamp.png", RunStartTimeDistribution(Board))
}

if (FALSE) {
    SavePlotAsPng("tank-success.png", TankSuccessByKey(Board))
}

if (FALSE) {
    SavePlotAsPng("overall-success-rate.png", SuccessRateBaseOnKeyLevel(Board))
}