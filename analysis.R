library(data.table)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)
library(extrafont)

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
    colClasses <- c("integer","factor","factor","factor","factor","numeric")
    result <- fread("data/static/affix-info.csv",
                    head=TRUE, sep=";", colClasses=colClasses, key=c("PeriodId"))
    return(result)
}

SpecInfo <- ReadSpecInfo()
ClassInfo <- ReadClassInfo()
DungeonInfo <- ReadDungeonInfo()
AffixInfo <- ReadAffixInfo()

#
SpecColorTable <- SpecInfo[ClassInfo, on = 'Class'][,.(Spec, Role, Color)]
SpecColors <- SpecColorTable$Color
names(SpecColors) <- SpecColorTable$Spec

# For each spec the color of choice.
TankColorTable <- SpecColorTable[Role == "Tank"]
TankColors <- TankColorTable$Color
names(TankColors) <- TankColorTable$Spec

# Not using classic white for priests for obvious reasons.
HealerColorTable <- SpecColorTable[Role == "Healer"]
HealerColorTable[Spec == "DisciplinePriest", Color := "#A9A9A9"]
HealerColorTable[Spec == "HolyPriest", Color := "#FFDF00"]
HealerColors <- HealerColorTable$Color
names(HealerColors) <- HealerColorTable$Spec

HealerShortNames <- c(
    "DisciplinePriest" = "Discipline",
    "RestorationShaman" = "Shaman",
    "HolyPaladin" = "Paladin",
    "HolyPriest" = "HolyPriest",
    "RestorationDruid" = "Druid",
    "MistweaverMonk" = "Monk"
)

TankShortNames <- c(
    "BloodDk" = "DK",
    "BrewmasterMonk" = "Monk",
    "GuardianDruid" = "Druid",
    "ProtectionPaladin" = "Paladin",
    "ProtectionWarrior" = "Warrior",
    "VengeanceDh" = "DH"
)

DpsShortNames <- c(
    "FrostDk" = "FrostDk",
    "UnholyDk" = "Unholy",
    "HavocDh" = "Havoc",
    "BalanceDruid" = "Balance",
    "FeralDruid" = "Feral",
    "BeastMasteryHunter" = "BM",
    "MarksmanshipHunter" = "MM",
    "SurvivalHunter" = "Survival",
    "ArcaneMage" = "Arcane",
    "FireMage" = "Fire",
    "FrostMage" = "Frost",
    "WindwalkerMonk" = "WW",
    "RetributionPaladin" = "Retri",
    "ShadowPriest" = "Shadow",
    "AssassinationRogue" = "Assassin",
    "OutlawRogue" = "Outlaw",
    "SubtletyRogue" = "Sub",
    "ElementalShaman" = "Elemental",
    "EnhancementShaman" = "Enhance",
    "AfflictionWarlock" = "Affli",
    "DemonologyWarlock" = "Demo",
    "DestructionWarlock" = "Desto",
    "ArmsWarrior" = "Arms",
    "FuryWarrior" = "Fury"
)

# print(AffixCombinations)
# exit(1)

SavePlotAsPng <- function(name, plot) {
    ggsave(name, plot = plot, width = 7, height = 4, type = "quartz")
}

RunDurationBoxplot <- function(Board) {
    Tbl <- Board[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    Tbl <- Tbl[KeystoneLevel >= 10]
    plot <- ggplot (Tbl, aes(x = reorder(Dungeon, TimeMinutes, median), y = TimeMinutes, color = Dungeon)) +
        stat_boxplot(geom ='errorbar', lwd=0.25) +
        geom_boxplot(lwd=0.25, outlier.size = 0.1) +
        geom_point(aes(x=Shorthand, y=TimeLimit, color=Shorthand), data = DungeonInfo, size = 2) +
        scale_y_log10() +
        guides(color=FALSE) +
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

TankPrecentage <- function(Board) {
    TankRuns <- Board[Tank != "",.(Count=.N), keyby=.(KeystoneLevel,Tank)]
    TankRuns[, Percent := sum(Count), by=KeystoneLevel]
    TankRuns[, Percent := Count/Percent]
    KeyRange <- 2:max(TankRuns$KeystoneLevel)
    plot <- ggplot(data=TankRuns,
        aes(x=KeystoneLevel, y=Percent, color=Tank)) +
        scale_color_manual(labels = TankShortNames, values = TankColors) +
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
        scale_color_manual(labels = HealerShortNames, values = HealerColors) +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        scale_y_continuous(labels = percent) +
        xlab("Keystone level") +
        ggtitle("Healers by keystone level")
    return (plot)
}

RunsByDungeon <- function(Board, L) {
    DungeonRuns <- Board[KeystoneLevel==L,.(Count=.N), keyby=.(Dungeon, Success)]
    # Not needed any more but here to remind myself of how to fill in missing values:
    # DungeonRuns <- DungeonRuns[CJ(Dungeon, Success, unique=TRUE)][is.na(Count), Count := 0]
    DungeonRuns <- DungeonRuns[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(DungeonRuns, aes(y=Count, x=reorder(Dungeon,-Count,sum), fill=Success)) +
        geom_bar(stat="identity") +
        ggtitle(paste0("Number of +", L, " runs by dungeon")) +
        xlab("Dungeon")
    return (plot)
}

KeystoneLevelHeatmap <- function(Board) {
    Summ <- Board[, .(Count = .N, SuccessRate=sum(Success) / .N), keyby=.(Dungeon, KeystoneLevel)]
    # TODO: 100 is just arbitrary limit for "too small sample"...
    Summ[, w := 1.0]
    Summ[Count <= 100, w := 0.8]
    Summ[Count <= 10, w := 0.6]
    Ord <- Board[, .(AvgSuccess = sum(Success) / .N), keyby=.(Dungeon)]
    Summ <- Summ[Ord, on = 'Dungeon']
    KeyRange <- 2:max(Summ$KeystoneLevel)
    Summ <- Summ[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Summ, aes(x=factor(KeystoneLevel),
                                 y=reorder(Dungeon, AvgSuccess, first))) +
        geom_tile(aes(fill = SuccessRate, width=w, height=w), colour = "white") +
        scale_fill_viridis(option = "cividis") +
        geom_text(aes(label = round(SuccessRate, 1)), size=2) +
        scale_x_discrete(breaks=KeyRange, labels=KeyRange) +
        coord_equal() +
        xlab("Keystone level") +
        ylab("Dungeon") +
        ggtitle("Success rate for each dungeon and keystone level combination")
    return (plot)
}

DayHeatmap <- function(Board) {
    Ord <- Board[KeystoneLevel == 10, .(AvgSuccess = sum(Success) / .N), keyby=.(Dungeon)]
    Tbl <- Board[KeystoneLevel == 10,
        .(SuccessRate=sum(Success) / .N, Day = floor_date(median(Datetime), "day")),
        keyby=.(Dungeon, DayNr)]
    Tbl <- Tbl[Ord, on = 'Dungeon']
    Tbl <- Tbl[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Tbl, aes(x=Day, y=reorder(Dungeon,AvgSuccess,first), fill = SuccessRate)) +
        geom_tile() +
        scale_fill_viridis(option = "cividis") +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        xlab("Time") +
        ylab("Dungeon") +
        ggtitle("Success rate in +10 dungeons over time")
    return (plot)
}

DayAverageHeatmap <- function(Board) {
    Tbl <- Board[Success == TRUE,
        .(AverageKeystoneLevel=mean(KeystoneLevel),
          Day = floor_date(median(Datetime), "day")),
        keyby=.(Dungeon, DayNr)]
    Tbl[,Ord := mean(AverageKeystoneLevel), by=Dungeon]
    Tbl <- Tbl[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(data=Tbl, aes(x=Day, y=reorder(Dungeon,Ord,first), fill = AverageKeystoneLevel)) +
        geom_tile() +
        scale_fill_viridis(option = "cividis") +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        xlab("Time") +
        ylab("Dungeon") +
        ggtitle("Average keystone level of completed dungeons")
    return (plot)
}

RunsPerDay <- function(Board) {
    Tbl <- Board[, .(Count = .N, Day = floor_date(first(Datetime), "day")), by=.(DayNr)]
    plot <- ggplot(data=Tbl, aes(x=Day, y=Count, color = wday(Day, label = TRUE))) +
        geom_point() +
        xlab("Time") +
        ylab("Number of runs") +
        labs(color="Day") +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        ggtitle("Number of daily runs over time")
    return (plot)
}

RunsPerWeek <- function(Board) {
    Summ <- Board[KeystoneLevel == 10,
        .(Count = .N,
          Week = min(floor_date(Datetime, "day"))),
        by=.(Success, WeekNr)]
    plot <- ggplot(data=Summ, aes(x = Week, y = Count, fill = Success)) +
        geom_bar(stat="identity") +
        scale_x_datetime() +
        xlab("Time") +
        ylab("Number of runs") +
        ggtitle("Number of +10 keystones each week")
    return (plot)
}

TankPrecentageOverTime <- function(Board) {
    Summ <- Board[
        Tank != "",
        .(Count=.N, Day = floor_date(first(Datetime), "day")),
        keyby=.(Tank, DayNr)]
    Summ[, Percent := sum(Count), by=DayNr]
    Summ[, Percent := Count/Percent]
    plot <- ggplot(data=Summ,
        aes(x=Day, y=Percent, color=Tank)) +
        scale_color_manual(labels = TankShortNames, values = TankColors) +
        geom_line() +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        scale_y_continuous(labels = percent) +
        xlab("Time") +
        ylab("Frequency") +
        ggtitle("Tank popularity over time")
    return (plot)
}

HealerPrecentageOverTime <- function(Board) {
    Summ <- Board[
        Healer != "",
        .(Count=.N, Day = floor_date(first(Datetime), "day")),
        keyby=.(Healer, DayNr)]
    Summ[, Percent:=sum(Count), by=DayNr]
    Summ[, Percent:=Count/Percent]
    plot <- ggplot(data=Summ, aes(x=Day, y=Percent, color=Healer)) +
        geom_line() +
        scale_color_manual(labels = HealerShortNames, values = HealerColors) +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        scale_y_continuous(labels = percent) +
        xlab("Time") +
        ylab("Frequency") +
        ggtitle("Healer popularity over time")
    return (plot)
}

AvgKeystonePerDay <- function(Board) {
    Tbl <- Board[
        Success == TRUE,
        .(AvgKeystoneLevel = mean(KeystoneLevel),
          PeriodId = first(PeriodId),
          Day = floor_date(first(Datetime), "day")),
        by=.(DayNr)]
    Tbl <- Tbl[AffixInfo, on = 'PeriodId']
    Tbl <- Tbl[! is.na(Day)]
    plot <- ggplot(data=Tbl, aes(x=Day, y=AvgKeystoneLevel, color=Affix3, shape=Affix2)) +
        geom_point() +
        xlab("Time") +
        ylab("Keystone level") +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        ggtitle("Average keystone level (completed in time)")
    return (plot)
}

RogueSuccess <- function(Board, MinKeystoneLevel) {
    Board[, HasRogue := (NumAssassinationRogue + NumOutlawRogue + NumSubtletyRogue) > 0]
    Summ <- Board[
        NumMelee <= 3 & KeystoneLevel >= MinKeystoneLevel & KeystoneLevel < 23,
        .(Percent=sum(Success) / .N),
        keyby=.(KeystoneLevel,HasRogue)]
    KeyRange <- 2:max(Summ$KeystoneLevel)
    plot <- ggplot(data=Summ, aes(x=KeystoneLevel, y=Percent, color=HasRogue)) +
        geom_line() +
        scale_x_continuous(breaks=KeyRange[KeyRange %% 5 == 0], minor_breaks=KeyRange) +
        scale_y_continuous(labels = percent) +
        xlab("Keystone level") +
        ylab("Success rate") +
        ggtitle("Average success rate depending on rogue")
    return (plot)
}

RunsByKeystoneLevel <- function(Board) {
    Summ <- Board[,.(Runs = .N), by=.(KeystoneLevel, Success)]
    KeyRange <- 2:max(Summ$KeystoneLevel)
    plot <- ggplot(data=Summ, aes(x = KeystoneLevel, y = Runs, fill = Success)) +
        geom_bar(stat="identity") +
        xlab("Keystone level") +
        ylab("Number of runs")
    return(plot)
}

AnimateRunCount <- function(Board) {
    Summ <- Board[KeystoneLevel <= 18,
        .(Runs = .N,
          Week = as.Date(floor_date(first(Datetime), "day"))),
        by=.(KeystoneLevel, Success, WeekNr)]

    plot <- ggplot(data=Summ, aes(x = factor(KeystoneLevel), y = Runs, fill = Success)) +
        geom_bar(stat="identity") +
        xlab("Keystone level") +
        ylab("Number of runs") +
        guides(fill=FALSE) +
        labs(title='Week: {frame_time}') +
        coord_cartesian(ylim=c(0,75000)) +
        transition_time(Week) +
        ease_aes('linear')

    N <- length(unique(Summ$WeekNr))
    anim <- animate(plot,
        device='png',
        fps = 1, nframes = N,
        width = 2000, height = 1200, res = 300)

    return (anim)
}

# i think this does not work too well
AnimateDungeons<- function(Board, L) {
    Summ <- Board[KeystoneLevel == L & DayNr < max(DayNr),
        .(Count = .N),
        keyby=.(Dungeon, Success, DayNr)]

    Summ <- Summ[CJ(Dungeon, Success, DayNr, unique=TRUE)][is.na(Count), Count := 0]

    Summ <- Summ[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]

    plot <- ggplot(data=Summ, aes(x = Dungeon, y = Count, fill = Success)) +
        geom_bar(stat="identity") +
        xlab("Dungeon") +
        ylab("Number of runs") +
        guides(fill=FALSE) +
        labs(title = 'Day: {frame_time}') +
        transition_time(DayNr) +
        ease_aes('linear')

    N <- length(unique(Summ$DayNr))
    anim <- animate(plot,
        device='png',
        fps = 7, nframes = N,
        width = 2000, height = 1200, res = 300)

    return (anim)
}

# Following function adds DayNr and WeekNr rows to database.
# This is pretty ugly code, sorry.
# This is bit tricky because different regions reset different times and we
# don't want for two different affix combinations have same DayNr or WeekNr.
# This means that two runs in different regions that have same time may have
# different DayNr. This is intentional.
AttachDayAndWeekNumbers <- function(Board) {
    DayIndex <- Board[, .(Day = min(Datetime), Dummy = 1), by=.(Region, PeriodId)]
    Offsets <- data.table(Offset = days(0 : 6), DayNr = c(1 : 7), Dummy = c(1,1,1,1,1,1,1))

    DayIndex <- Offsets[DayIndex, on = 'Dummy', mult = 'all', allow.cartesian=TRUE]
    DayIndex[, Dummy := NULL]
    DayIndex[, DayBegin := Day + Offset]
    DayIndex[, DayEnd := DayBegin + days(1)]
    DayIndex[, Offset := NULL]
    DayIndex[, Day := NULL]
    setkeyv(DayIndex, c("Region", "PeriodId", "DayBegin", "DayEnd"))

    Board[, Datetime2 := Datetime]
    Board <- foverlaps(Board, DayIndex,
        by.x = c("Region", "PeriodId", "Datetime", "Datetime2"),
        by.y = c("Region", "PeriodId", "DayBegin", "DayEnd"),
        type = "within",
        mult = "first")
    Board[, Datetime2 := NULL]
    Board[, WeekNr := PeriodId - min(PeriodId) + 1]
    Board[, DayNr := DayNr + 7*(WeekNr - 1)]
    return (Board)
}

#
# Read DB and print some useful information:
#

cat("Reading database...\n")
Board <- readRDS("db.rds")
# Board <- readRDS("db-season1.rds")
cat(paste0("    Loaded ", nrow(Board), " rows\n"))

cat("    Attaching day and week numbers...\n")
Board <- AttachDayAndWeekNumbers(Board)

cat("    Rebuilding database keys...\n")
setkeyv(Board, c(key(Board), "WeekNr", "DayNr"))

cat("Plotting...\n")

#
# General style:
#

theme_set(theme_linedraw() + theme(text = element_text(family = 'Caladea')))

#
# Working on some ideas:
#

if (FALSE) {
    Board[DungeonInfo, on='Dungeon',
        Score := (1.08 ^ (KeystoneLevel - 1))*(TimeMinutes/TimeLimit)]
    # log(Score) = (KeystoneLevel - 1)*log(1.08)) + log(TimeMinutes) - log(TimeLimit)
    plot <- ggplot(data=Board, aes(x = log(Score), color = Success)) +
        geom_density()

    SavePlotAsPng("test.png", plot)

    stop("ohno")
}

#
# Correlating with specs:
#

if (TRUE) {
    Indicators <- data.table(
        Indicator = c("NumFrostDk", "NumUnholyDk", "NumHavocDh", "NumBalanceDruid", "NumFeralDruid", "NumBeastMasterHunter", "NumMarksmanshipHunter", "NumSurvivalHunter", "NumArcaneMage", "NumFireMage", "NumFrostMage", "NumWindwalkerMonk", "NumRetributionPaladin", "NumShadowPriest", "NumAssassinationRogue", "NumOutlawRogue", "NumSubtletyRogue", "NumElementalShaman", "NumEnhancementShaman", "NumAfflictionWarlock", "NumDemonologyWarlock", "NumDestructionWarlock", "NumArmsWarrior", "NumFuryWarrior"),
        Spec = c("FrostDk", "UnholyDk", "HavocDh", "BalanceDruid", "FeralDruid", "BeastMasteryHunter", "MarksmanshipHunter", "SurvivalHunter", "ArcaneMage", "FireMage", "FrostMage", "WindwalkerMonk", "RetributionPaladin", "ShadowPriest", "AssassinationRogue", "OutlawRogue", "SubtletyRogue", "ElementalShaman", "EnhancementShaman", "AfflictionWarlock", "DemonologyWarlock", "DestructionWarlock", "ArmsWarrior", "FuryWarrior")
    )

    Indicators <- Indicators[SpecInfo, on = 'Spec']
    Indicators <- Indicators[! is.na(Indicator)]

    Summ <- Indicators[,
        Board[WeekNr == max(WeekNr) & KeystoneLevel >= 10,
            .(Odds = sum(Success) / .N),
            by=.(Has = (get(Indicator) > 0))],
        keyby = .(Indicator)]
    Summ <- dcast(Summ, ... ~ Has, value.var="Odds")
    Summ <- Summ[Indicators, on = 'Indicator']
    Summ[, Indicator := NULL]
    setnames(Summ, "TRUE", "OddsWith")
    setnames(Summ, "FALSE", "OddsWithout")

    # Summ[, Advantage := OddsWith / OddsWithout]
    Summ[, Advantage := OddsWith - OddsWithout]

    # shift <- scales::trans_new("shift", transform = function(x) {x-1}, inverse = function(x) {x+1})
    plot <- ggplot(data=Summ, aes(x = reorder(Spec, Advantage), y = Advantage, fill = Spec)) +
        geom_bar(stat = "identity", colour="black", size = 0.1) +
        scale_x_discrete(labels = DpsShortNames) +
        # scale_y_continuous(trans = shift) +
        scale_fill_manual(values = SpecColors) +
        guides(fill=FALSE) +
        xlab(NULL) +
        ylab("Success chance difference") +
        ggtitle("Advantage to having spec in group for +10 and higher") +
        scale_y_continuous(labels = percent) +
        coord_flip()

    SavePlotAsPng("advantage.png", plot)
}

#
# Animations:
#

if (FALSE) {
    library(gganimate)

    # anim_save(AnimateDungeons(Board, 10), file = 'dungeons-animation.gif')
    anim_save(AnimateRunCount(Board), file = 'runs-animation.gif')
}

#
# Bunch of graphs:
#

if (TRUE) {
    # Week <- Board[WeekNr == 11] # worst week
    # Week <- Board[WeekNr == 14] # best week
    Week <- Board[WeekNr == max(WeekNr)] # latest week
    FirstTime <- min(Week$Datetime)
    LastTime <- max(Week$Datetime)
    cat("    Plotting statistics of a week.\n")
    cat(paste0("       First run ", FirstTime, "\n"))
    cat(paste0("       Last run  ", LastTime, "\n"))

    SavePlotAsPng("keystone-level-heatmap.png", KeystoneLevelHeatmap(Week))
    SavePlotAsPng("tank-precentage.png", TankPrecentage(Week))
    SavePlotAsPng("healer-precentage.png", HealerPrecentage(Week))
    SavePlotAsPng("success-by-dungeon-15.png", RunsByDungeon(Week, 10))
    SavePlotAsPng("duration-boxplot.png", RunDurationBoxplot(Week))
    SavePlotAsPng("run-count.png", RunsByKeystoneLevel(Week))
}

#
# Summary data across entire timespan of M+:
#

if (FALSE) {
    cat("    Plotting statistics of entire time period.\n")
    cat(paste0("       First run ", min(Board$Datetime), "\n"))
    cat(paste0("       Last run  ", max(Board$Datetime), "\n"))

    SavePlotAsPng("rogue-success.png", RogueSuccess(Board, 2))
    SavePlotAsPng("day-heatmap.png", DayHeatmap(Board))
    SavePlotAsPng("day-average-heatmap.png", DayAverageHeatmap(Board))
    SavePlotAsPng("healers-season.png", HealerPrecentageOverTime(Board))
    SavePlotAsPng("tanks-season.png", TankPrecentageOverTime(Board))
    SavePlotAsPng("timestamp.png", RunsPerDay(Board))
    SavePlotAsPng("10-runs-per-week.png", RunsPerWeek(Board))
    SavePlotAsPng("keystone-level.png", AvgKeystonePerDay(Board))
}