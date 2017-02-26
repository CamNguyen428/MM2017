
# Quick and crappy model of predicting wins in a tournament using reg. season point differential

sampleSubmission <- fread("sample_submission.csv")
library(data.table)
library(caret)

regSeason <- fread("RegularSeasonCompactResults.csv")

# Calculate point differential for each game
regSeason[, pointDiff := Wscore - Lscore]

# Calculate point differential for each team by season, wins by more than 20 count as a win by 20
wins <- regSeason[, .(pdplus = mean(min(pointDiff, 20))), by = .(Season, Wteam)]
loss <- regSeason[, .(pdminus = mean(min(pointDiff, 20))), by = .(Season, Lteam)]
point_diff <- merge(wins, loss, by.x = c("Season", "Wteam"), by.y = c("Season", "Lteam"))
point_diff[, pd:= pdplus - pdminus][, c("pdplus", "pdminus") := NULL]
rm(wins, loss)

# Manipulate tourney results to train data
tourney <- fread("TourneyCompactResults.csv")
tourney_wins <- tourney[, .(Season = Season,
                            team1 = ifelse(Wteam < Lteam, Wteam, Lteam), 
                            team2 = ifelse(Wteam > Lteam, Wteam, Lteam),
                            Wteam = Wteam)][, win := as.numeric(team1 == Wteam)][,Wteam := NULL]

tourney_pd <- merge(tourney_wins, point_diff, by.x = c("Season", "team1"), by.y = c("Season", "Wteam"))
tourney_pd <- merge(tourney_pd, point_diff, by.x = c("Season", "team2"), by.y = c("Season", "Wteam"))
tourney_pd <- tourney_pd[, .(pd = pd.x, opd = pd.y, win = factor(win, labels = c("L", "W")))]

# Split the data into a train and test set; 80-20 split
samp <- createDataPartition(tourney_pd$win, p = 0.8, list = F)
trainset <- tourney_pd[samp]
testvals <- tourney_pd[-samp, win]
testset <- tourney_pd[-samp, .(pd = pd, opd = opd)]

trainctrl <- trainControl(method = "repeatedcv",
                          number = 10, 
                          repeats = 10)

model1 <- train(win ~ ., data = trainset,
                method = "plr",
                trControl = trainctrl)

predictions <- predict(model1, testset, type = "prob")

myLogloss <- function(probs, vals) {
    if(is.factor(vals)) {
        vals <- as.numeric(vals)
    }
    -1 * mean(vals * log(probs$W) + (1 - vals) * log(1 - probs$L))
}

# plot the learning curve
plr_data <- learing_curve_dat(dat = trainset, outcome = "win",
                              test_prop = 1/4,
                              method = "plr",
                              trControl = trainctrl)

ggplot(plr_data, aes(x = Training_Size, y = Accuracy, color = Data)) + geom_smooth() + theme_bw()
