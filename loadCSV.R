library(data.table)


ReadDungeonInfo <- function() {
    colClasses <- c("factor","factor","integer")
    result <- fread("data/static/dungeon-info.csv",
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses,
                    key=c("Dungeon"))
    return(result)
}

ReadPeriodIndex <- function() {
    colClasses <- c("factor","integer","integer64", "integer64")
    result <- fread("data/static/period-index.csv",
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses,
                    key=c("Region", "StartTimestamp", "EndTimestamp"))
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
    result <- fread(fname,
                    head=TRUE,
                    sep=";",
                    colClasses=colClasses)
    return(result)
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

    # Clear useless fields added by foverlaps
    Tbl[, StartTimestamp := NULL]
    Tbl[, EndTimestamp := NULL]
    Tbl[, Timestamp2 := NULL]

    Tbl[,NumMelee:=NumFrostDk+NumUnholyDk+NumHavocDh+NumFeralDruid+NumSurvivalHunter+NumWindwalkerMonk+NumRetributionPaladin+NumAssassinationRogue+NumOutlawRogue+NumSubtletyRogue+NumEnhancementShaman+NumArmsWarrior+NumFuryWarrior]
    Tbl[,TimeMinutes:=Duration/60000]

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
    Tbl[, Success:=TimeMinutes<=TimeLimit]
    Tbl[, TimeLimit:=NULL]

    Tbl[, Datetime:=as.POSIXct(Timestamp/1000, origin="1970-01-01", tz="UTC")]

    # Remove few more redundant columns
    Tbl[,Timestamp:=NULL]
    Tbl[,Duration:=NULL]

    # This is weird:
    setcolorder(Tbl, c("Region", "PeriodId", "Faction", "Dungeon", "KeystoneLevel"))
    return(Tbl)
}

args <- commandArgs(TRUE)


Db <- NULL
# setDTthreads()
DbFileName <- "db.rds"

# XXX
if (file.exists(DbFileName)) {
    Db <- readRDS(DbFileName)
}

for (file in args) {
    cat(paste0("Loading file ", file, "\n"))
    Tbl <- ReadCSVFile(file)
    Tbl <- Preprocess(Tbl)

    if (is.null(Db)) {
        Db <- unique(Tbl)
    }
    else {
        Db <- funion(Db, Tbl)
    }
}

setkeyv(Db, c("Region", "PeriodId", "Faction", "Dungeon", "KeystoneLevel", "Success"))

saveRDS(Db, DbFileName)
