rm(list = ls()); gc()
library(data.table)
library(dplyr)
library(caret)
##### Build Simple Model #####

##### Read in the tournament data #####
# Tournament Data
tourney <- fread("TourneyDetailedResults.csv")
t2016 <- tourney[Season == 2016]
trnOld <- tourney[Season < 2013]

##### Stat Calculating Functions #####
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
convertTrn <- function(dt) {
    dt <- copy(dt)
    dt[, id := paste(Season, ifelse(Wteam > Lteam, Lteam, Wteam), ifelse(Wteam < Lteam, Lteam, Wteam), sep = "_")]
    dt[, label := as.numeric(Wteam < Lteam)]
    dt[, .(id, label)]
}
getStats <- function(current, history = fread("RegularSeasonDetailedResults.csv")) {
    historyStats <- compressStats(history)
    historyStats[, id := paste(Season, Team, sep = "_")]
    historyStats$Season <- historyStats$Team <- NULL
    current <- copy(current)
    current[, team1 := gsub("_[0-9]+$", "", id)]
    current[, team2 := gsub("_[0-9]+_", "_", id)]
    ret <- merge(current, historyStats, all.x = T, by.x = "team1", by.y ="id")
    ret <- merge(ret, historyStats, all.x = T, by.x = "team2", by.y ="id", suffixes = c(".1", ".2"))
    ret
}

# Create features using season data
generateFeatures <- function(season = fread("RegularSeasonDetailedResults.csv")) {
    getPointDiffs <- function() {
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
        tidySeason[, id := paste(Season, Team, sep = "_")][, Ascore := mean(Ascore), by = id]
        
        tidySeason <- tidySeason[, .(team_id = id, Ascore)] %>% unique
    }
    ret <- getPointDiffs() %>% merge(getAdjustedPointDiff(), by = "team_id")
}

# Create features using team stats
addFeatures <- function(dt, history = fread("RegularSeasonDetailedResults.csv")) {
    createFreeThrowPercentage <- function(dt) {
        dt[, Wftp.1 := Wftm.1/Wfta.1]
        dt[, Wftp.2 := Wftm.2/Wfta.2]
        dt[, Wftm.1 := NULL]
        dt[, Wfta.1 := NULL]
        dt[, Wftm.2 := NULL]
        dt[, Wfta.2 := NULL]
    }
    addSeeds <- function(dt) {
        seeds <- fread("TourneySeeds.csv")
        seeds[, id:= paste(Season, Team, sep = "_")]
        seeds[, SeedNum := gsub('[a-zA-Z]', '', Seed) %>% as.numeric]
        temp <- merge(dt, seeds[, .(id, SeedNum)], by.x = "team1", by.y = "id")
        temp <- merge(temp, seeds[, .(id, SeedNum)], by.x = "team2", by.y = "id", suffixes = c(".1", ".2"))
        dt[, Seed.1 := temp$SeedNum.1]
        dt[, Seed.2 := temp$SeedNum.2]
    }
    addAggressiveness <- function(dt, history = history) {
        temp <- calcFeat(history, Wfta^2 + Wor^2 + Wto^2 + Wpf^2, "Aggressiveness")
        temp <- copy(temp)
        temp[, id := paste(Season, Team, sep = "_")][, Aggressiveness := sqrt(Aggressiveness)]
        combined <- merge(dt, temp, by.x = "team1", by.y = "id")
        combined <- merge(combined, temp, by.x = "team2", by.y = "id", suffixes = c(".1", ".2"))
        dt[, Aggressiveness.1 := combined$Aggressiveness.1]
        dt[, Aggressiveness.2 := combined$Aggressiveness.2]
    }
    addGeneratedFeatures <- function(dt) {
        additionalFeatures <- generateFeatures(history)
        temp <- merge(dt, additionalFeatures, by.x = "team1", by.y = "team_id", all.x = T)
        temp <- merge(temp, additionalFeatures, by.x = "team2", by.y = "team_id", suffixes = c(".1", ".2"), all.x = T)
        for(feat in names(additionalFeatures)) {
            if(feat == "team_id") {
                next()
            }
            for(suff in c(".1", ".2")) {
                featName <- paste0(feat, suff)
                vals <- temp[, get(featName)]
                dt[, featName := vals, with = FALSE]
            }
        }
    }
    createFreeThrowPercentage(dt)
    #addSeeds(dt)
    #addAggressiveness(dt)
    addGeneratedFeatures(dt)
}

##### Model Building Function #####
buildModel <- function(training, method = "glm") {
    loglossSummary <- function(data, lev = NULL, model = NULL)  {  
        #print(paste(class(data$pred))) # factor  
        data$pred <- as.numeric(data$pred)-1 # otherwise I get an error as these are factors  
        data$obs <- as.numeric(data$obs)-1 # otherwise I get an error as these are factors  
        epsilon      <- .000000000000001  
        yhat           <- pmin(pmax(data$pred, rep(epsilon)), 1-rep(epsilon))  
        logloss      <- -mean(data$obs*log(yhat) + (1-data$obs)*log(1 - yhat))  
        names(logloss) <- "LOGLOSS"  
        logloss  
    }  
    traindf <- as.data.frame(training)[, -(1:3)]
    traindf$label <- as.factor(traindf$label)
    tc <- trainControl(method = "cv", number = 5, summaryFunction = loglossSummary)
    train(label ~ ., data = traindf, method = method, trControl = tc, metric = "LOGLOSS", maximize = F)
}

##### Model Evaluation Functions #####
evalModel <- function(model, testing = NULL, labeled = T) {
    if(is.null(testing)) {
        testing <- convertTrn(t2016) %>% getStats()
        addFeatures(testing)
    }
    pred1 <- predict(model, testing, type = "prob")
    if(!labeled) {
        return(pred1[, '1'])
    }
    evaldf <- data.frame(label = testing$label, pred = pred1[, '1'])
    evaldf$logloss <- -(evaldf$label * log(evaldf$pred) + (1 - evaldf$label) * log(1 - evaldf$pred))
    print(paste("Log Loss:", mean(evaldf$logloss)))
    print(paste("Accuracy:", 1 - mean(abs(evaldf$label - round(evaldf$pred)))))
}
#####

##### Create training set from old tournament data and regular season games between tournament teams #####
regOld <- fread("RegularSeasonDetailedResults.csv") %>% 
    .[Season < 2013] %>% 
    .[Wteam %in% c(trnOld$Wteam, trnOld$Lteam) & Lteam %in% c(trnOld$Wteam, trnOld$Lteam)]
training <- convertTrn(trnOld %>% rbind(regOld)) %>% getStats()
addFeatures(training)

##### Build and Evaluate the Model #####
m1 <- buildModel(training)
#m2 <- buildModel(training, method = "xgbLinear") # Log loss increases on submission data

evalModel(m1)
#evalModel(m2)

submission <- fread("sample_submission.csv") %>% getStats()
addFeatures(submission)
labels <- fread("TourneyDetailedResults.csv") %>% convertTrn()
submission <- merge(submission, labels, by = "id")
evalModel(m1, submission)

# Log Loss: .6436
# Accuracy: .7015

# Compare to benchmark
library(MLmetrics)
bm <- fread("SeedLinearModelSubmission.csv") %>% merge(labels, by = "id")
paste("Benchmark Logloss:", LogLoss(bm$pred, bm$label))
paste("My Logloss:", LogLoss(evalModel(m1, submission, labeled = F), submission$label))
