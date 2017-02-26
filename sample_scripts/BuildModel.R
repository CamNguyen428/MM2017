# Build 
rm(list = ls()); gc()

library(data.table)
library(tidyverse)


# Converts the detailed data to the sample submission format, used for tournament data
convertTrn <- function(dt) {
        dt <- copy(dt)
        dt[, id := paste(Season, ifelse(Wteam > Lteam, Lteam, Wteam), ifelse(Wteam < Lteam, Lteam, Wteam), sep = "_")]
        dt[, label := as.numeric(Wteam < Lteam)]
        dt[, .(id, label)]
    }

# Calculates season stats for all major basketball stat categories
buildReferenceData <- function(reference = fread("RegularSeasonDetailedResults.csv")) {
    # Helper function: Uses string to calculate average for statistic, ie "Wscore" gets ppg
    calcFeatWithString <- function(dt, featStr) {
        wFeat <- dt[, .(wfeat = mean(get(featStr)), nwins = .N), by = c("Season", "Wteam")]
        featStr2 <- gsub("^W", "L", featStr)
        lFeat <- dt[, .(lfeat = mean(get(featStr2)), nloss = .N), by = c("Season", "Lteam")]
        feat <- merge(wFeat, lFeat, by.x = c("Season", "Wteam"), by.y = c("Season", "Lteam"), all = T)
        feat[is.na(feat)] <- 0
        feat[, ft := (wfeat*nwins +  lfeat*nloss)/ (nwins+nloss)]
        ret <- feat[, .(Season, Wteam, ft)]
        names(ret) <- c("Season", "Team", featStr)
        ret
    }
    # Calculates season stats for all major basketball stat categories
    compressStats <- function(dt) {
        dt <- copy(dt)
        dt[, Wloc := NULL]
        statNames <- grep("^W", names(dt), value = T)
        statNames <- statNames[statNames != "Wteam"]
        ret <- dt[, .(Season, Team = Wteam)] %>% unique()
        for(stat in statNames) {
            ret <- merge(ret,
                         calcFeatWithString(dt, stat), by = c("Season", "Team"), all = T)
        }
        return(ret)
    }
    return(compressStats(reference))
}

# Combines season data with formatted tournament data
# Params: current = data.table in sample submission format, history = season data to use
getStats <- function(current, history = fread("RegularSeasonDetailedResults.csv")) {
    historyStats <- buildReferenceData(history)
    historyStats[, id := paste(Season, Team, sep = "_")]
    historyStats$Season <- historyStats$Team <- NULL
    current <- copy(current)
    current[, team1 := gsub("_[0-9]+$", "", id)]
    current[, team2 := gsub("_[0-9]+_", "_", id)]
    ret <- merge(current, historyStats, all.x = T, by.x = "team1", by.y ="id")
    ret <- merge(ret, historyStats, all.x = T, by.x = "team2", by.y ="id", suffixes = c(".1", ".2"))
    ret
}

# Uses season data to calculate average stats
generateFeatures <- function(season = fread("RegularSeasonDetailedResults.csv")) {
    # Helper function: Calculates average basketball statistics using an expression
    calcFeat <- function(dt, featCalc, featName = "feat") {
        arguments <- as.list(match.call())
        wFeat <- dt[, .(wfeat = mean(eval(arguments$featCalc, dt)), nwins = .N), by = c("Season", "Wteam")]
        dtc <- copy(dt)
        names(dtc) <- gsub("^W", "l", names(dtc))
        names(dtc) <- gsub("^L", "W", names(dtc))
        names(dtc) <- gsub("^l", "L", names(dtc))
        lFeat <- dtc[, .(lfeat = mean(eval(arguments$featCalc, dtc)), nloss = .N), by = c("Season", "Wteam")]
        
        feat <- merge(wFeat, lFeat, by = c("Season", "Wteam"), all = T)
        feat[is.na(feat)] <- 0
        feat[, ft := (wfeat*nwins +  lfeat*nloss)/ (nwins+nloss)]
        names(feat) <- c("Season", "Team", paste0("w.", featName), "nwins", paste0("l.", featName), "nloss", featName)
        feat
    }
    
    # Calculates point differential and scaled point differential for each team
    getPointDiffs <- function(season = fread("RegularSeasonDetailedResults.csv")) {
        pointDiff <- calcFeat(season, Wscore - Lscore, "PointDiff") %>% copy()
        pointDiff[, team_id := paste(Season, Team, sep = "_")]
        teamWins <- pointDiff[, .(Season, Team, Wnwins = nwins, Wnloss = nloss)]
        season_wins <- merge(season, teamWins, by.x = c("Season", "Wteam"), by.y = c("Season", "Team"))
        teamWins <- pointDiff[, .(Season, Team, Lnwins = nwins, Lnloss = nloss)]
        season_wins <- merge(season_wins, teamWins, by.x = c("Season", "Lteam"), by.y = c("Season", "Team"))
        scaledPointDiff <- calcFeat(season_wins, 
                                    (Wscore - Lscore) * Lnwins * (Wnwins + Wnloss) / Wnwins / (Lnwins + Lnloss), 
                                    "ScaledPointDiff") %>% copy()
        scaledPointDiff[, team_id := paste(Season, Team, sep = "_")]
        return(merge(pointDiff[, .(team_id, PointDiff)], 
                     scaledPointDiff[, .(team_id, ScaledPointDiff)], 
                     by = "team_id"))
    }
    getAdjustedPointDiff <- function() {
        source("timepointdiff.R")
        tidySeason <- getTidySeason()
        tidySeason[, team_id := paste(Season, Team, sep = "_")][
            , alpha := fitSES(Ascore), by = team_id][
                , Ascore := mean(Ascore), by = team_id]
        tidySeason[, c("Season", "Team", "Daynum") := NULL]
        tidySeason <- tidySeason %>% unique
    }
    ret <- getPointDiffs() %>% merge(getAdjustedPointDiff(), by = "team_id")
}