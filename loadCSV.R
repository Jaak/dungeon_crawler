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
    Tbl[DungeonInfo, on='Dungeon', Success:=TimeMinutes<=TimeLimit]
    Tbl[, Datetime:=as.POSIXct(Timestamp/1000, origin="1970-01-01", tz="UTC")]

    # Remove few more redundant columns
    Tbl[,Timestamp:=NULL]
    Tbl[,Duration:=NULL]
    return(Tbl)
}

args <- commandArgs(TRUE)


Db <- NULL
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
