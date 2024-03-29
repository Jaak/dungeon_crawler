suppressMessages({
    library(data.table)
    library(ggplot2)
    library(viridis)
    library(scales)
    library(lubridate)
    library(zoo)
})

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
    colClasses <- c("character", "factor","factor")
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

ReadDungeonTimeLimits <- function() {
    colClasses <- c("factor","integer","integer", "integer")
    result <- fread("data/static/dungeon-time-limits.csv",
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses,
                    key=c("Dungeon", "PeriodStart", "PeriodEnd"))
    return(result)
}

SpecInfo <- ReadSpecInfo()
ClassInfo <- ReadClassInfo()
DungeonInfo <- ReadDungeonInfo()
AffixInfo <- ReadAffixInfo()
DungeonTimeLimits <- ReadDungeonTimeLimits()

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

DpsColumns <- c(
    "NumFrostDk",
    "NumUnholyDk",
    "NumHavocDh",
    "NumBalanceDruid",
    "NumFeralDruid",
    "NumBeastMasterHunter",
    "NumMarksmanshipHunter",
    "NumSurvivalHunter",
    "NumArcaneMage",
    "NumFireMage",
    "NumFrostMage",
    "NumWindwalkerMonk",
    "NumRetributionPaladin",
    "NumShadowPriest",
    "NumAssassinationRogue",
    "NumOutlawRogue",
    "NumSubtletyRogue",
    "NumElementalShaman",
    "NumEnhancementShaman",
    "NumAfflictionWarlock",
    "NumDemonologyWarlock",
    "NumDestructionWarlock",
    "NumArmsWarrior",
    "NumFuryWarrior"
)

Season1EndDate <- as.POSIXct("2019-01-23", tz = "UTC")
Season2EndDate <- as.POSIXct("2019-07-10", tz = "UTC")
Season3EndDate <- as.POSIXct("2020-01-22", tz = "UTC")
Season4EndDate <- as.POSIXct("2021-07-06", tz = "UTC")

Patches <- c(
    "8.1"   = as.POSIXct("2018-11-14", tz = "UTC"),
    "8.1.5" = as.POSIXct("2019-03-13", tz = "UTC"),
    "8.2"   = as.POSIXct("2019-06-26", tz = "UTC"),
    "8.2.5" = as.POSIXct("2019-09-25", tz = "UTC"),
    "8.3"   = as.POSIXct("2020-01-15", tz = "UTC"),
    "8.3.7" = as.POSIXct("2020-07-21", tz = "UTC"),
    "9.0.5" = as.POSIXct("2021-03-09", tz = "UTC"),
    "9.1"   = as.POSIXct("2021-06-29", tz = "UTC")
)

Seasons <- c(
    "BFA S2" = Season1EndDate,
    "BFA S3" = Season2EndDate,
    "BFA S4" = Season3EndDate,
    "S2" = Season4EndDate
)


# print(AffixCombinations)
# exit(1)

SavePlotAsPng <- function(name, plot) {
    ggsave(name, plot = plot, width = 7, height = 4, type = "quartz")
}

# This is bitrotten:
# RunDurationBoxplot <- function(Board, L) {
#     Title <- paste0("Median run length of +", L, " (compared to time limit)")
#     Tbl <- Board[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
#     Tbl <- Tbl[KeystoneLevel == L]
#     plot <- ggplot (Tbl, aes(x = reorder(Dungeon, TimeMinutes, median), y = TimeMinutes, color = Dungeon)) +
#         stat_boxplot(geom ='errorbar', lwd=0.25) +
#         geom_boxplot(lwd=0.25, outlier.size = 0.1) +
#         geom_point(aes(x=Shorthand, y=TimeLimit, color=Shorthand), data = DungeonInfo, size = 2) +
#         scale_y_log10(breaks = c(20, 30, 40, 60, 120)) +
#         guides(color=FALSE) +
#         xlab("Dungeon") +
#         ylab("Time (in minutes)") +
#         ggtitle (Title)
#
#     return (plot)
# }

NumberOfRuns <- function(frame) {
    summary <- frame[, list(Runs = .N), keyby=.(KeystoneLevel,Dungeon)]
    plot <- ggplot(data=summary, aes(x = factor(KeystoneLevel), y = Runs)) +
        geom_bar(stat="identity") +
        xlab("Keystone level") +
        ggtitle("Number of runs per keystone level")

    return (plot)
}

RunsByDungeon <- function(Board, L) {
    Title <- paste0("Number of +", L, " runs by dungeon")
    DungeonRuns <- Board[KeystoneLevel==L,.(Count=.N), keyby=.(Dungeon, Success)]
    # Not needed any more but here to remind myself of how to fill in missing values:
    # DungeonRuns <- DungeonRuns[CJ(Dungeon, Success, unique=TRUE)][is.na(Count), Count := 0]
    DungeonRuns <- DungeonRuns[DungeonInfo, on = 'Dungeon', Dungeon := Shorthand]
    plot <- ggplot(DungeonRuns, aes(y=Count, x=reorder(Dungeon,-Count,sum), fill=Success)) +
        geom_bar(stat="identity") +
        ggtitle(Title) +
        xlab("Dungeon")
    return (plot)
}

RefreshDungeonTimeLimits <- function(Tbl) {
    # Tbl[, TimeLimit := NULL]
    Tbl[, PeriodId2 := PeriodId]
    Tbl <- foverlaps(Tbl, DungeonTimeLimits,
        by.x = c("Dungeon", "PeriodId", "PeriodId2"),
        by.y = c("Dungeon", "PeriodStart", "PeriodEnd"),
        type = "within",
        mult = "first")
    if (anyNA(Tbl$TimeLimit)) {
        cat("WARNING! For some dungeons time limit is missing! Update dungeon-time-limits.csv file.")
    }

    Tbl[, PeriodId2 := NULL]
    Tbl[, PeriodStart := NULL]
    Tbl[, PeriodEnd := NULL]
    return(Tbl)
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

DayHeatmap <- function(Board, L) {
    Ord <- Board[KeystoneLevel == L, .(AvgSuccess = sum(Success) / .N), keyby=.(Dungeon)]
    Tbl <- Board[KeystoneLevel == L,
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
        ggtitle(paste0("Success rate in +", L, " dungeons over time"))
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
    Tbl <- Board[, .(Count = .N, Day = floor_date(median(Datetime), "day")), by=.(DayNr)]
    Tbl <- Tbl[Day != max(Day)]
    Tbl[, Segment := 0]
    for (SeasonName in names(Seasons)) {
        Patch <- Seasons[[SeasonName]]
        Tbl[, Segment := Segment + (Day < Patch)]
    }

    plot <- ggplot(data=Tbl, aes(x=Day, y=Count))
    plot <- AddDefaultDatetimeAxis(plot) +
        geom_point(aes(color = wday(Day, label = TRUE))) +
        geom_smooth(aes(group = factor(Segment)),
            color = "gray", size = 0.5, method = 'lm', formula = y ~ x, se = FALSE) +
        ylab("Number of runs") +
        labs(color="Day") +
        ggtitle("Number of daily runs over time")
    return (plot)
}

RunsPerWeek <- function(Board, Level) {
    Title <- paste0("Number of +", Level, " runs each week")
    Summ <- Board[KeystoneLevel == Level,
        .(Count = .N,
          Week = min(floor_date(Datetime, "day"))),
        keyby=.(Success, WeekNr)]
    # Summ[, Week := factor(Week)]
    # print(Summ)
    plot <- ggplot(data=Summ, aes(x = Week, y = Count, fill = Success)) +
        geom_vline(xintercept = Season1EndDate, size = 0.25) +
        geom_bar(stat="identity", width = 500000) +
        scale_x_datetime(breaks = date_breaks("1 month"), date_labels = "%b") +
        xlab("Time") +
        ylab("Number of runs") +
        ggtitle(Title)
    return (plot)
}

AvgKeystonePerDay <- function(Board) {
    Tbl <- Board[
        Success == TRUE,
        .(AvgKeystoneLevel = mean(KeystoneLevel),
          PeriodId = first(PeriodId),
          Day = floor_date(first(Datetime), "day")),
        by=.(DayNr)]
    Tbl <- Tbl[DayNr != max(DayNr)]
    Tbl <- Tbl[AffixInfo, on = 'PeriodId']
    Tbl <- Tbl[! is.na(Day)]
    plot <- ggplot(data=Tbl, aes(x=Day, y=AvgKeystoneLevel, color=Affix3, shape=Affix2))
    plot <- AddDefaultDatetimeAxis(plot) +
        geom_point() +
        ylab("Keystone level") +
        ggtitle("Average keystone level (completed in time)")
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

AddDefaultDatetimeAxis <- function(p) {
    return (
        p +
        geom_vline(xintercept = Patches, size = 0.25, linetype = "dashed") +
        geom_vline(xintercept = Seasons, size = 0.25) +
        scale_x_datetime(
            breaks = date_breaks("1 month"),
            date_labels = "%b",
            sec.axis = dup_axis(
                breaks = Seasons,
                labels = names(Seasons)
            )
        ) +
        theme(axis.title.x = element_blank())
    )
}

TankPercentageOverTime <- function(Board, top = NULL, smooth = FALSE) {
    Title <- "Tank popularity over time"
    if (! is.null(top)) {
        Board <- Board[Success == TRUE]
        setorder(Board, DayNr, Dungeon, -KeystoneLevel, TimeMinutes)
        Board <- Board[, .SD[, .SD[.I < .N * top]], by = .(Dungeon, DayNr)]
        Title <- paste0(Title, " (top ", round(100*top), "% of runs)")
    }

    Summ <- Board[
        Tank != "",
        .(Count=.N, Day = mean(Datetime)),
        keyby=.(Tank, DayNr)]
    Summ[, Percent := sum(Count), by=DayNr]
    Summ[, Percent := Count/Percent]

    Summ[, SizeType := "Small"]
    Summ[, ColorType := Tank]
    Summ$Group <- 2*rank(Summ$Tank, ties.method="min") + 1
    Summ[, Tank := NULL]

    if (smooth) {
        Summ[, Percent := rollmean(Percent, 7, fill = NA), by=.(Group)]
        Summ <- na.omit(Summ, cols="Percent")
    }

    Black <- copy(Summ)
    Black[, ColorType := "Black"]
    Black[, SizeType := "Mid"]
    Black[, Group := Group - 1]

    Summ <- funion(Black, Summ)

    plot <- ggplot(data=Summ, aes(x=Day, y=Percent, group=Group, color=ColorType, size=SizeType))
    plot <- AddDefaultDatetimeAxis(plot) +
        geom_line() +
        scale_size_manual(values = c("Mid" = 1.0, "Small" = 0.8)) +
        scale_color_manual(
            breaks = c(NA, names(TankShortNames)),
            labels = c("Black" = NA, TankShortNames),
            values = c("Black" = "#000000", TankColors)) +
        scale_y_continuous(labels = percent) +
        guides(size = "none", color = guide_legend("Tank")) +
        ylab("Frequency") +
        ggtitle(Title)
    return (plot)
}

HealerPercentageOverTime <- function(Board, top = NULL, smooth = FALSE) {

    Title <- "Healer popularity over time"
    if (! is.null(top)) {
        Board <- Board[Success == TRUE]
        setorder(Board, DayNr, Dungeon, -KeystoneLevel, TimeMinutes)
        Board <- Board[, .SD[, .SD[.I < .N * top]], by = .(Dungeon, DayNr)]
        Title <- paste0(Title, " (top ", round(100*top), "% of runs)")
    }

    # Aggregate:
    Summ <- Board[
        Healer != "",
        .(Count=.N, Day = floor_date(first(Datetime), "day")),
        keyby=.(Healer, DayNr)]
    Summ[, Percent:=sum(Count), by=DayNr]
    Summ[, Percent:=Count/Percent]

    # Following is for drawing lines that have black outline.
    # Also achieves effect that outlined lines are drawn one after another
    Summ[, SizeType := "Small"]
    Summ[, ColorType := Healer]
    Summ$Group <- 2*rank(Summ$Healer, ties.method="min") + 1
    Summ[, Healer := NULL]

    if (smooth) {
        Summ[, Percent := rollmean(Percent, 7, fill = NA), by=.(Group)]
        Summ <- na.omit(Summ, cols="Percent")
    }

    # Black line is drawn bigger and before each colored line!
    Black <- copy(Summ)
    Black[, ColorType := "Black"]
    Black[, SizeType := "Mid"]
    Black[, Group := Group - 1]

    Summ <- funion(Black, Summ)

    plot <- ggplot(data=Summ, aes(x=Day, y=Percent, group=Group, color=ColorType, size=SizeType));
    plot <- AddDefaultDatetimeAxis(plot) +
        geom_line() +
        scale_size_manual(values = c("Mid" = 1.0, "Small" = 0.8)) +
        scale_color_manual(
            breaks = c(NA, names(HealerShortNames)),
            labels = c("Black" = NA, HealerShortNames),
            values = c("Black" = "#000000", HealerColors)) +
        scale_y_continuous(labels = percent) +
        guides(size = "none", color = guide_legend("Healer")) +
        ylab("Frequency") +
        ggtitle(Title)
    return (plot)
}

DpsPercentageOverTime <-
    function(Board,
             top = NULL,
             smooth = FALSE,
             MinLevel = NULL,
             discard = NULL,
             role = NULL)
{
    Title <- "DPS spec popularity over time"
    Indicators <- data.table(
        Indicator = c("NumFrostDk", "NumUnholyDk", "NumHavocDh", "NumBalanceDruid", "NumFeralDruid", "NumBeastMasterHunter", "NumMarksmanshipHunter", "NumSurvivalHunter", "NumArcaneMage", "NumFireMage", "NumFrostMage", "NumWindwalkerMonk", "NumRetributionPaladin", "NumShadowPriest", "NumAssassinationRogue", "NumOutlawRogue", "NumSubtletyRogue", "NumElementalShaman", "NumEnhancementShaman", "NumAfflictionWarlock", "NumDemonologyWarlock", "NumDestructionWarlock", "NumArmsWarrior", "NumFuryWarrior"),
        Spec = c("FrostDk", "UnholyDk", "HavocDh", "BalanceDruid", "FeralDruid", "BeastMasteryHunter", "MarksmanshipHunter", "SurvivalHunter", "ArcaneMage", "FireMage", "FrostMage", "WindwalkerMonk", "RetributionPaladin", "ShadowPriest", "AssassinationRogue", "OutlawRogue", "SubtletyRogue", "ElementalShaman", "EnhancementShaman", "AfflictionWarlock", "DemonologyWarlock", "DestructionWarlock", "ArmsWarrior", "FuryWarrior")
    )

    Indicators <- Indicators[SpecInfo, on = 'Spec']
    Indicators <- Indicators[! is.na(Indicator)]

    if (! is.null(role)) {
        if (tolower(role) == "melee") {
            Indicators <- Indicators[Role == "Melee"]
            Title <- paste("Melee", Title)
        }

        if (tolower(role) == "ranged") {
            Indicators <- Indicators[Role == "Ranged"]
            Title <- paste("Ranged", Title)
        }
    }


    Tbl <- Board

    if (! is.null(top)) {
        setorder(Tbl, DayNr, Dungeon, -KeystoneLevel, TimeMinutes)
        Tbl <- Tbl[, .SD[, .SD[.I < .N * top]], by = .(Dungeon, DayNr)]
        Title <- paste0(Title, " (top ", round(100*top), "% of runs)")
    }

    if (is.null(MinLevel)) {
        MinLevel <- 2
    }
    else {
        Title <- paste0(Title, " (+", MinLevel, " and higher)")
    }

    Tbl <- Tbl[KeystoneLevel >= MinLevel]

    # TODO: super slow join:
    Summ <- Indicators[,
        Tbl[,
            .(Percent = sum(get(Indicator) > 0) / .N,
              # Count = .N,
              Spec = Spec,
              Day = median(Datetime)),
            by=.(DayNr)],
        by = .(Indicator)]

    if (! is.null(discard)) {
        Summ[, Discard := mean(Percent < 0.1), by = .(Spec)]
        Summ <- Summ[Discard < discard]
        Summ[, Discard := NULL]
    }

    # Following is for drawing lines that have black outline.
    # Also achieves effect that outlined lines are drawn one after another
    Summ[, SizeType := "Small"]
    Summ[, ColorType := Spec]
    Summ$Group <- 2*rank(Summ$Spec, ties.method="min") + 1
    Summ[, Spec := NULL]

    if (smooth) {
        Summ[, Percent := rollmean(Percent, 7, fill = NA), by=.(Group)]
        Summ <- na.omit(Summ, cols="Percent")
    }

    # Black line is drawn bigger and before each colored line!
    Black <- copy(Summ)
    Black[, ColorType := "Black"]
    Black[, SizeType := "Mid"]
    Black[, Group := Group - 1]

    Summ <- funion(Black, Summ)

    plot <- ggplot(data=Summ, aes(x=Day, y=Percent, group=Group, color=ColorType, size=SizeType))
    plot <- AddDefaultDatetimeAxis(plot) +
        geom_line() +
        scale_size_manual(values = c("Mid" = 1.0, "Small" = 0.8)) +
        scale_color_manual(values = c("Black" = "#000000", SpecColors)) +
        scale_y_continuous(labels = percent) +
        ylab("Proportion of runs with spec present") +
        ggtitle(Title) +
        theme(legend.position="none")
    return (plot)
}

# For each spec plot the success chance difference
# between having spec in the group vs not having it.
DpsSpecAdvantage <- function(Board, MinLevel) {
    # Ugh, workaround for poor naming decisions...
    Indicators <- data.table(
        Indicator = c("NumFrostDk", "NumUnholyDk", "NumHavocDh", "NumBalanceDruid", "NumFeralDruid", "NumBeastMasterHunter", "NumMarksmanshipHunter", "NumSurvivalHunter", "NumArcaneMage", "NumFireMage", "NumFrostMage", "NumWindwalkerMonk", "NumRetributionPaladin", "NumShadowPriest", "NumAssassinationRogue", "NumOutlawRogue", "NumSubtletyRogue", "NumElementalShaman", "NumEnhancementShaman", "NumAfflictionWarlock", "NumDemonologyWarlock", "NumDestructionWarlock", "NumArmsWarrior", "NumFuryWarrior"),
        Spec = c("FrostDk", "UnholyDk", "HavocDh", "BalanceDruid", "FeralDruid", "BeastMasteryHunter", "MarksmanshipHunter", "SurvivalHunter", "ArcaneMage", "FireMage", "FrostMage", "WindwalkerMonk", "RetributionPaladin", "ShadowPriest", "AssassinationRogue", "OutlawRogue", "SubtletyRogue", "ElementalShaman", "EnhancementShaman", "AfflictionWarlock", "DemonologyWarlock", "DestructionWarlock", "ArmsWarrior", "FuryWarrior")
    )

    Indicators <- Indicators[SpecInfo, on = 'Spec']
    Indicators <- Indicators[! is.na(Indicator)]

    Title <- paste0("Advantage to having spec in group for +", MinLevel, " and higher")

    Summ <- Indicators[,
        Board[KeystoneLevel >= MinLevel,
            .(Odds = sum(Success) / .N, Count = .N),
            by=.(Has = (get(Indicator) > 0))],
        keyby = .(Indicator)]
    Summ <- dcast(Summ, ... ~ Has, value.var=c("Odds", "Count"))
    Summ <- Summ[Indicators, on = 'Indicator']
    Summ[, Indicator := NULL]
    setnames(Summ, "Odds_TRUE", "OddsWith")
    setnames(Summ, "Odds_FALSE", "OddsWithout")
    setnames(Summ, "Count_TRUE", "CountWith")
    setnames(Summ, "Count_FALSE", "CountWithout")

    # Odds ratio:
    # Summ[, Advantage := OddsWith / OddsWithout]
    # Standard error for odds ratio is bit non-trivial to calculate
    # shift <- scales::trans_new("shift", transform = function(x) {x-1}, inverse = function(x) {x+1})

    # Odds difference:
    Summ[, Advantage := OddsWith - OddsWithout]
    Summ[, SE := sqrt(OddsWith*(1 - OddsWith)/CountWith + OddsWithout*(1 - OddsWithout)/CountWithout)]

    plot <- ggplot(data=Summ, aes(x = reorder(Spec, Advantage), y = Advantage, fill = Spec)) +
        geom_bar(stat = "identity", colour="black", size = 0.1) +
        geom_errorbar(aes(ymin = Advantage - 1.96*SE, ymax = Advantage + 1.96*SE), colour="black", size = 0.2, width = 0.8) +
        geom_errorbar(aes(ymin = Advantage - 1.28*SE, ymax = Advantage + 1.28*SE), colour="black", size = 0.2, width = 0.4) +
        scale_x_discrete(labels = DpsShortNames) +
        # scale_y_continuous(trans = shift) +
        scale_fill_manual(values = SpecColors) +
        guides(fill=FALSE) +
        xlab(NULL) +
        ylab("Success chance difference") +
        ggtitle(Title) +
        scale_y_continuous(labels = percent) +
        coord_flip()

    return (plot)
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

cat("Charting...\n")

### Temp <- Board[, .(Count = .N), keyby = c("Tank", "Healer", DpsColumns)]
### cat(paste0("Runs without tank or healer : ", nrow(Temp[Healer == "" & Tank == ""]), "\n"))
### cat(paste0("Runs without healer : ", nrow(Temp[Healer == ""]), "\n"))
### cat(paste0("Runs without tank : ", nrow(Temp[Tank == ""]), "\n"))
###
### Temp <- Temp[Healer != "" & Tank != ""]
###
### setorder(Temp, -Count)
### cat(paste0("Unique group compositions : ", nrow(Temp), "\n"))
### print(head(Temp$Count))
### exit()
###
#
# General style:
#

theme_set(theme_linedraw())

# Disable the following conditional if you don't want to bother with fancy fonts.
if (TRUE) {
    suppressMessages(library(extrafont))
    theme_set(theme_linedraw() + theme(text = element_text(family = 'Caladea')))
}

#
# Bunch of graphs for specific weeks:
#

if (TRUE) {
    # Season 1:
    # Week <- Board[WeekNr == 11] # worst week
    # Week <- Board[WeekNr == 14] # best week

    # Season 2:
    # WeekNr == 41 is 2019-06-12
    # Week <- Board[WeekNr == 33] # in memory of bursting quaking
    # Week <- Board[WeekNr == 34] # best week raging volcanic
    # Week <- Board[WeekNr == 35] # teeming explosive as comparison to previous season worst

    Week <- Board[WeekNr + 1 == max(WeekNr)] # latest week

    FirstTime <- min(Week$Datetime)
    LastTime <- max(Week$Datetime)

    cat("    Statistics of a week.\n")
    cat(paste0("       First run ", FirstTime, "\n"))
    cat(paste0("       Last run  ", LastTime, "\n"))

    # p <- 0.25
    # Top <- Week[Success == TRUE]
    # setorder(Top, DayNr, Dungeon, -KeystoneLevel, TimeMinutes)
    # Top <- Top[, .SD[, .SD[.I < .N * p]], by = .(Dungeon, DayNr)]
    # print(summary(Top$KeystoneLevel))

    SavePlotAsPng("keystone-level-heatmap.png", KeystoneLevelHeatmap(Week))
    SavePlotAsPng("success-by-dungeon.png", RunsByDungeon(Week, 15))
    # SavePlotAsPng("duration-boxplot.png", RunDurationBoxplot(Week, 10))
    SavePlotAsPng("run-count.png", RunsByKeystoneLevel(Week))
    SavePlotAsPng("advantage.png", DpsSpecAdvantage(Week, 15))
}

#
# Summary data across entire timespan of M+:
#

if (FALSE) {
    cat("    Statistics of entire time period.\n")
    cat(paste0("       First run ", min(Board$Datetime), "\n"))
    cat(paste0("       Last run  ", max(Board$Datetime), "\n"))

    # SavePlotAsPng("dps-season.png", DpsPercentageOverTime(Board, top = 0.25))
    SavePlotAsPng("melee-dps-season.png", DpsPercentageOverTime(Board, top = 0.25, role = "melee"))
    SavePlotAsPng("ranged-dps-season.png", DpsPercentageOverTime(Board, top = 0.25, role = "ranged"))
    # SavePlotAsPng("day-heatmap.png", DayHeatmap(Board, 15))
    # SavePlotAsPng("day-average-heatmap.png", DayAverageHeatmap(Board))
    SavePlotAsPng("healers-season.png", HealerPercentageOverTime(Board, top = 0.25))
    SavePlotAsPng("tanks-season.png", TankPercentageOverTime(Board))
    SavePlotAsPng("timestamp.png", RunsPerDay(Board))
    # SavePlotAsPng("runs-per-week.png", RunsPerWeek(Board, 15))
    SavePlotAsPng("keystone-level.png", AvgKeystonePerDay(Board))
    # SavePlotAsPng("advantage.png", DpsSpecAdvantage(Board, 15))
}
