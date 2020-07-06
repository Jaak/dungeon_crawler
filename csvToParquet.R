library(data.table)
library(arrow, warn.conflicts = FALSE)

ReadDungeonInfo <- function() {
    colClasses <- c("character","character")
    result <- fread("data/static/dungeon-info.csv",
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses,
                    key=c("Dungeon"))
    return(result)
}

ReadPeriodIndex <- function() {
    colClasses <- c("character","integer","integer64", "integer64")
    result <- fread("data/static/period-index.csv",
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses,
                    key=c("Region", "StartTimestamp", "EndTimestamp"))
    return(result)
}

ReadDungeonTimeLimits <- function() {
    colClasses <- c("character","integer","integer", "integer")
    result <- fread("data/static/dungeon-time-limits.csv",
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses,
                    key=c("Dungeon", "PeriodStart", "PeriodEnd"))
    return(result)
}

ReadCSVFile <- function(fname) {
    colClasses <-
        c("character","character","character",
          "integer64","integer","integer",
          "character","character")
    colClasses <- c(colClasses, rep("integer", times = 4*6))
    result <- fread(fname,
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses)
    return(unique(result))
}

DungeonInfo <- ReadDungeonInfo()
PeriodIndex <- ReadPeriodIndex()
DungeonTimeLimits <- ReadDungeonTimeLimits()

Preprocess <- function(Tbl) {
    Tbl[, Timestamp2 := Timestamp]

    Tbl <- foverlaps(Tbl, PeriodIndex,
        by.x = c("Region", "Timestamp", "Timestamp2"),
        by.y = c("Region", "StartTimestamp", "EndTimestamp"),
        type = "within",
        mult = "first")

    MillisecondsInADay <- 86400000
    Tbl[, DayId := floor((Timestamp - StartTimestamp) / MillisecondsInADay)]
    if (any(Tbl$DayId > 6)) {
        cat("ERROR! DayId > 6.")
        quit("no")
    }

    # Last decimal digit of DayId indicates the number of days since weekly reset.
    # Weekday can be derived from Timestamp column, but do note that all regions
    # cover multiple timezones.
    Tbl[, DayId := PeriodId*10 + DayId]

    # Clear useless fields added by foverlaps
    Tbl[, StartTimestamp := NULL]
    Tbl[, EndTimestamp := NULL]
    Tbl[, Timestamp2 := NULL]
    MillisecondsInAMinute <- 60000
    Tbl[,TimeMinutes:=Duration/MillisecondsInAMinute]

    Tbl[, PeriodId2 := PeriodId]
    Tbl <- foverlaps(Tbl, DungeonTimeLimits,
        by.x = c("Dungeon", "PeriodId", "PeriodId2"),
        by.y = c("Dungeon", "PeriodStart", "PeriodEnd"),
        type = "within",
        mult = "first")
    if (anyNA(Tbl$TimeLimit)) {
        cat("ERROR! Missing time limit! Update dungeon-time-limits.csv file.")
        quit("no")
    }

    Tbl[, PeriodId2 := NULL]
    Tbl[, PeriodStart := NULL]
    Tbl[, PeriodEnd := NULL]
    Tbl[, Success:=TimeMinutes<=TimeLimit]
    Tbl[, TimeLimit:=NULL]
    Tbl[, Timestamp:=as.POSIXct(Timestamp/1000, origin="1970-01-01", tz="UTC")]
    Tbl[,Duration:=NULL]

    setcolorder(Tbl, c("Region", "PeriodId", "DayId", "Faction", "Dungeon", "KeystoneLevel", "Timestamp", "TimeMinutes", "Success"))
    setorder(Tbl, Region, PeriodId, DayId, Timestamp)
    return(Tbl)
}

SaveAsParquet <- function(df, name) {
    dbSchema <- schema(
        Region = string(),
        PeriodId = int32(),
        DayId = int32(),
        Faction = string(),
        Dungeon = string(),
        KeystoneLevel = int8(),
        Timestamp = timestamp("ms", timezone = "UTC"),
        TimeMinutes = float32(),
        Success = bool(),
        Tank = string(),
        Healer = string(),
        NumFrostDk = int8(),
        NumUnholyDk = int8(),
        NumHavocDh = int8(),
        NumBalanceDruid = int8(),
        NumFeralDruid = int8(),
        NumBeastMasterHunter = int8(),
        NumMarksmanshipHunter = int8(),
        NumSurvivalHunter = int8(),
        NumArcaneMage = int8(),
        NumFireMage = int8(),
        NumFrostMage = int8(),
        NumWindwalkerMonk = int8(),
        NumRetributionPaladin = int8(),
        NumShadowPriest = int8(),
        NumAssassinationRogue = int8(),
        NumOutlawRogue = int8(),
        NumSubtletyRogue = int8(),
        NumElementalShaman = int8(),
        NumEnhancementShaman = int8(),
        NumAfflictionWarlock = int8(),
        NumDemonologyWarlock = int8(),
        NumDestructionWarlock = int8(),
        NumArmsWarrior = int8(),
        NumFuryWarrior = int8()
    )

    Tbl <- Table$create(df, schema = dbSchema)
    write_parquet(Tbl, name)
}

args <- commandArgs(TRUE)

for (file in args) {
    cat(paste0("Loading file ", file, "\n"))
    Df <- ReadCSVFile(file)
    Df <- Preprocess(Df)
    outFileName <- gsub(".csv$", ".parquet", file)
    SaveAsParquet(Df, outFileName)
}
