
library(data.table)
library(tidyverse)
library(forecast)
# Look at adjusted point diff as time series


regSeason <- fread("RegularSeasonCompactResults.csv")

getTidySeason <- function() {
    # Get ppg and oppg for each team
    calcFeat <- function(dt, featCalc, featName = "feat") {
        # Get arguments
        arguments <- as.list(match.call())
        # Calculate number of wins and mean of arg when winning for each team
        wFeat <- dt[, .(wfeat = mean(eval(arguments$featCalc, dt)), nwins = .N), by = c("Season", "Wteam")]
        
        # Change the table so names that start with W start with L and vice versa
        dtc <- copy(dt)
        names(dtc) <- gsub("^W", "l", names(dtc))
        names(dtc) <- gsub("^L", "W", names(dtc))
        names(dtc) <- gsub("^l", "L", names(dtc))
        
        # Calculate number of losses and mean of arg when losing for each team
        lFeat <- dtc[, .(lfeat = mean(eval(arguments$featCalc, dtc)), nloss = .N), by = c("Season", "Wteam")]
        
        # Combine tables for wins and losses
        feat <- merge(wFeat, lFeat, by = c("Season", "Wteam"), all = T)
        feat[is.na(feat)] <- 0
        feat[, ft := (wfeat*nwins +  lfeat*nloss)/ (nwins+nloss)]
        names(feat) <- c("Season", "Team", paste0("w.", featName), "nwins", paste0("l.", featName), "nloss", featName)
        feat
    }
    
    ppg <- calcFeat(regSeason, Wscore, "ppg")[, .(Season, Team, ppg)]
    oppg <- calcFeat(regSeason, Lscore, "oppg")[, .(Season, Team, oppg)]
    
    wppg <- copy(ppg)
    names(wppg) <- c("Season", "Wteam", "Wppg")
    seasonWithppg <- merge(regSeason, wppg, by = c("Season", "Wteam"))
    lppg <- wppg
    names(lppg) <- c("Season", "Lteam", "Lppg")
    seasonWithppg <- merge(seasonWithppg, lppg, by = c("Season", "Lteam"))
    
    woppg <- copy(oppg)
    names(woppg) <- c("Season", "Wteam", "Woppg")
    seasonWithppg <- merge(seasonWithppg, woppg, by = c("Season", "Wteam"))
    loppg <- woppg
    names(loppg) <- c("Season", "Lteam", "Loppg")
    seasonWithppg <- merge(seasonWithppg, loppg, by = c("Season", "Lteam"))
    
    # Adjust teams score based on ppg
    seasonWithppg[, Wascore := 2 * Wscore / (Wppg + Loppg)]
    seasonWithppg[, Lascore := 2 * Lscore / (Woppg + Lppg)]
    
    # Tidy up the dataset
    tidySeason <- seasonWithppg[, .(Season, Team = Lteam, Daynum, Ascore = Lascore)] %>%
        rbind(seasonWithppg[, .(Season, Team = Wteam, Daynum, Ascore = Wascore)]) %>%
        arrange(Season, Team, Daynum) %>% data.table()
    
    tidySeason
}

tidySeason <- getTidySeason()
ggplot(tidySeason[Season > 2008][Team == 1242]) + geom_line(aes(x = Daynum, y = Ascore)) + facet_wrap(~ Season)

