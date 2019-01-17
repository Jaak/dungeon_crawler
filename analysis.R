library(data.table)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)

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

AffixInfo[, IsDifficultWeek := (Affix1 == "Fortified") & (Affix2 == "Sanguine" | Affix2 == "Teeming")]

# For each spec the color of choice
SpecColorTable <- SpecInfo[ClassInfo, on = 'Class'][,.(Spec, Role, Color)]
TankColorTable <- SpecColorTable[Role == "Tank"]
TankColors <- TankColorTable$Color
names(TankColors) <- TankColorTable$Spec

# print(AffixCombinations)
# exit(1)

SavePlotAsPng <- function(name, plot) {
    ggsave(name, plot = plot, device = png(), height = 4)
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

RunsPerDay <- function(Board) {
    Tbl <- Board[,
        .(Count = .N, PeriodId = last(PeriodId), Day = floor_date(first(Datetime), "day")),
        by=.(d = as.IDate(Datetime))]
    plot <- ggplot(data=Tbl, aes(x=Day, y=Count,
                                 color=wday(Day, label = TRUE))) +
        geom_point() +
        xlab("Time") +
        ylab("Number of runs (in a day)") +
        labs(color="Day") +
        scale_x_datetime(breaks = date_breaks("1 month"))
    return (plot)
}

AvgKeystonePerDay <- function(Board) {
    Tbl <- Board[
        Success == TRUE,
        .(AvgKeystoneLevel = mean(KeystoneLevel),
          PeriodId = first(PeriodId),
          Day = floor_date(first(Datetime), "day")),
        by=.(as.IDate(Datetime))]
    Tbl <- Tbl[AffixInfo, on = 'PeriodId']
    Tbl <- Tbl[! is.na(Day)]
    plot <- ggplot(data=Tbl, aes(x=Day, y=AvgKeystoneLevel, color=Affix2)) +
        geom_point() +
        xlab("Time") +
        ylab("Average level of completed keystone") +
        scale_x_datetime(
            breaks = date_breaks("1 month"),
            date_labels = "%b")
    return (plot)
}

#
# Read DB and print some useful information:
#

cat("Reading database...\n")
Board <- readRDS("db.rds")
cat(paste0("    Loaded ", nrow(Board), " rows\n"))
cat("Database contains following time periods:\n")

# Summ <- Board[, .(FirstRun = min(Datetime), LastRun = max(Datetime)), by=.(Region, PeriodId)]
# Summ <- Summ[, data.table(Day = days(0:6)), by=PeriodId]
# print(Summ)
# print(nrow(Summ))
# exit(1)

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
    LatestWeek <- Board[PeriodId == max(PeriodId)]
    FirstTime <- min(LatestWeek$Datetime)
    LastTime <- max(LatestWeek$Datetime)
    cat(paste0("Plotting runs from ", FirstTime, " to ", LastTime, "\n"))

    SavePlotAsPng("number-of-runs.png", NumberOfRuns(LatestWeek))
    SavePlotAsPng("tank-precentage.png", TankPrecentage(LatestWeek))
    SavePlotAsPng("healer-precentage.png", HealerPrecentage(LatestWeek))
    SavePlotAsPng("melee-precentage.png", MeleeCountPrecentage(LatestWeek))
    SavePlotAsPng("success-by-dungeon-15.png", SuccessRateByDungeon(LatestWeek, 15))
    SavePlotAsPng("keystone-level-heatmap.png", KeystoneLevelHeatmap(LatestWeek))
    SavePlotAsPng("duration-boxplot.png", RunDurationBoxplot(LatestWeek))

    SavePlotAsPng("healer-heatmap.png", HealerHeatmap(Board, 15))
    SavePlotAsPng("tank-heatmap.png", TankHeatmap(Board, 15))
}

#
# Summary data across entire timespan of M+:
#

if (FALSE) {
    SavePlotAsPng("timestamp.png", RunsPerDay(Board))
    SavePlotAsPng("keystone-level.png", AvgKeystonePerDay(Board))
}


#
# A bit boring but still little informative plots:
#

if (FALSE) {
    SavePlotAsPng("dungeon-popularity.png", DungeonPopularity(Board))
    SavePlotAsPng("tank-success.png", TankSuccessByKey(Board))
    SavePlotAsPng("overall-success-rate.png", SuccessRateBaseOnKeyLevel(Board))
}