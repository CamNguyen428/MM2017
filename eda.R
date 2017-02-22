rm(list = ls())

# Exploring the data
library(ggplot2)
library(data.table)
library(dplyr)
library(caret)

teams <- fread("Teams.csv")
# Season data
regSeason <- fread("RegularSeasonDetailedResults.csv")
regOld <- regSeason[Season != 2016]
reg2016 <- regSeason[Season == 2016]
# Tournament Data
tourney <- fread("TourneyDetailedResults.csv")
t2016 <- tourney[Season == 2016]
trnOld <- tourney[Season != 2016]

rm(tourney)

##### Examining Point Differential #####
calcPointDiff <- function(dt) {
    pointDiffW <- dt[, .(Wpd = mean(Wscore - Lscore)), by = Wteam]
    pointDiffL <- dt[, .(Lpd = mean(Lscore - Wscore)), by = Lteam]
    
    setkey(pointDiffW, Wteam)
    setkey(pointDiffL, Lteam)
    
    pointDiff <- pointDiffW[pointDiffL][, .(Team = Wteam, 
                                            pd = ifelse(is.na(Wpd), 0, Wpd) + ifelse(is.na(Lpd), 0, Lpd))]
}
#####

##### Examining Bonus Possessions (OR, Opp turnovers) #####
calcBonusPoss <- function(dt) {
    wBonusPoss <- dt[, .(wbp = mean(Wor + Lto)), by = Wteam]
    lBonusPoss <- dt[, .(lbp = mean(Lor + Wto)), by = Lteam]
    
    setkey(wBonusPoss, Wteam)
    setkey(lBonusPoss, Lteam)
    
    bp <- wBonusPoss[lBonusPoss][, .(Team = Wteam,
                                     bp = ifelse(is.na(wbp), 0, wbp) + ifelse(is.na(lbp), 0, lbp))]
}
#####

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
# Can't take in combinations of variables, i.e. Wor + Dor
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

##### Examining point differential in reg season and tournament wins #####
rpd <- calcFeat(regOld, Wscore - Lscore, featName = "pointDiff")
tpd <- calcFeat(trnOld, Wscore - Lscore, featName = "pointDiff")
pd <- merge(tpd, rpd, all.x = T, by = c("Season", "Team"), suffixes = c(".t", ".r"))
ggplot(data = pd, aes(y = pointDiff.r, x = nwins.t %>% as.factor)) + geom_boxplot() + 
    xlab("Tournament wins") + ylab("Reg. Season Point Differential") + facet_wrap(~ Season)
rm(rpd, tpd, pd)
#####

##### Clustering to create different types of teams by offense #####
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

# Get offensive stats for each team by season
regByTeam <- compressStats(regSeason)
regByTeam[, id := paste(Season, Team, sep = "_")][, Season := NULL][, Team := NULL]
regTeamdf <- as.data.frame(regByTeam) %>% subset(select = -id)

# Cluster teams by regular season offensive performance
#regTeamdf <- scale(regTeamdf)
clusters <- hclust(dist(regTeamdf))
clust <- cutree(clusters, 5)
clusteredTeams <- regByTeam[, .(id = id, clust = clust)]
rm(regByTeam, regTeamdf, clusters, clust)

# EDA: Examine tournament wins by cluster
trnOldByTeam <- calcFeat(trnOld, Wscore)
trnOldByTeam <- trnOldByTeam[, .(id = paste(Season, Team, sep = "_"), wins = nwins)]
trnClusters <- merge(trnOldByTeam, clusteredTeams, by = "id")
table(trnClusters$wins, trnClusters$clust) %>% addmargins()
rm(trnOldByTeam, trnClusters)
#####

##### Collect tables by cluster #####
#####

# I want to use the current regular season to find teams who have played similar to the teams of interest in the past,
# then I'll use old tournament data from those teams to make predictions.

###### Making a simpler model first #####

# Convert tournament data to id, win
convertTrn <- function(dt) {
    dt <- copy(dt)
    dt[, id := paste(Season, ifelse(Wteam > Lteam, Lteam, Wteam), ifelse(Wteam < Lteam, Lteam, Wteam), sep = "_")]
    dt[, label := as.numeric(Wteam < Lteam)]
    dt[, .(id, label)]
}
# Given id, win table, use ids to get team stats
getStats <- function(current, history) {
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

# Add more features
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
addAggressiveness <- function(dt, history) {
    temp <- calcFeat(history, Wfta^2 + Wor^2 + Wto^2 + Wpf^2, "Aggressiveness")
    temp <- copy(temp)
    temp[, id := paste(Season, Team, sep = "_")][, Aggressiveness := sqrt(Aggressiveness)]
    combined <- merge(dt, temp, by.x = "team1", by.y = "id")
    combined <- merge(combined, temp, by.x = "team2", by.y = "id", suffixes = c(".1", ".2"))
    dt[, Aggressiveness.1 := combined$Aggressiveness.1]
    dt[, Aggressiveness.2 := combined$Aggressiveness.2]
}

training <- convertTrn(trnOld) %>% getStats(regOld)
createFreeThrowPercentage(training)
addSeeds(training)
#addAggressiveness(training, regOld)
traindf <- as.data.frame(training)[, -(1:3)]
traindf$label <- as.factor(traindf$label)
model <- glm(label ~ ., data = traindf, family = binomial)

trainctrl <- trainControl(method = "repeatedcv",
                          number = 10, 
                          repeats = 3)
model2 <- train(label ~ ., data = traindf, method = "glmnet", trControl = trainctrl)

testing <- convertTrn(t2016) %>% getStats(reg2016)
createFreeThrowPercentage(testing)
addSeeds(testing)
#addAggressiveness(testing, reg2016)
testdf <- as.data.frame(testing)[, -(1:4)]
predictions <- predict(model, testdf, type = "response")
predictions2 <- predict(model2, testdf, type = "prob")
paste("Accuracy = ", 1 - ((predictions %>% round()) - testing$label) %>% abs %>% mean)
paste("Accuracy = ", 1 - ((predictions2[,'1'] %>% round()) - testing$label) %>% abs %>% mean)

#.6567 Without free throw percentage
#.6716 with free throw percentage

# Look at model performance
df <- data.frame(pred = predictions, val = testing$label) %>% cbind(testing$id)
df2 <- data.frame(pred = predictions2[, '1'], val = testing$label) %>% cbind(testing$id)
df$log <- -(df$val * log(df$pred) + (1 - df$val) * log(1 - df$pred))
df2$log <- -(df2$val * log(df2$pred) + (1 - df2$val) * log(1 - df2$pred))

df[order(df$log, decreasing = T),] %>% head
print(mean(df$log))
print(mean(df2$log))

#####

sampleSubmission <- fread("sample_submission.csv")
subStats <- getStats(sampleSubmission, regSeason)
subStats %>% createFreeThrowPercentage()
subStats %>% addSeeds()
subdf <- as.data.frame(subStats)[,-(1:4)]
pred <- predict(model, subdf, type = "response")

submission <- data.frame(id = sampleSubmission$id, pred = pred)
write.csv(submission, "sub1.csv", row.names = F, quote = F)
