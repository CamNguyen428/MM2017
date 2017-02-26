# Sample

#load required libraries
library(readr) #data import
library(stringr) #string manipulation
library(dplyr) #data wrangling
library(reshape) #more string manipulation

#load data
tourney_seeds <- read_csv("TourneySeeds.csv")
tourney_compact_results <- read_csv("TourneyCompactResults.csv")
sample_submission <- read_csv("sample_submission.csv")

seed.lm.train <- tourney_compact_results %>%
    select(Season,
           Wteam,
           Lteam) %>%
    filter(Season < 2013)

seed.lm.train$FirstTeam <- pmin(seed.lm.train$Wteam, seed.lm.train$Lteam)
seed.lm.train$SecondTeam <- pmax(seed.lm.train$Wteam, seed.lm.train$Lteam)
seed.lm.train$FirstTeamWin <- 0
seed.lm.train$FirstTeamWin[seed.lm.train$Wteam==seed.lm.train$FirstTeam] <- 1

seed.lm.train <- seed.lm.train %>%
    left_join(tourney_seeds, by=c("Season", "FirstTeam" = "Team")) %>%
    left_join(tourney_seeds, by=c("Season", "SecondTeam" = "Team"), suffix=c(".FirstTeam", ".SecondTeam"))

seed.lm.train$Seed.FirstTeam <- str_extract(seed.lm.train$Seed.FirstTeam, "[:digit:]+")
seed.lm.train$Seed.SecondTeam <- str_extract(seed.lm.train$Seed.SecondTeam, "[:digit:]+")

seed.lm.train$Seed.FirstTeam <- as.numeric(seed.lm.train$Seed.FirstTeam)
seed.lm.train$Seed.SecondTeam <- as.numeric(seed.lm.train$Seed.SecondTeam)
seed.lm.train$SeedDistance <- seed.lm.train$Seed.SecondTeam - seed.lm.train$Seed.FirstTeam

seed.lm.model <- lm(FirstTeamWin ~ SeedDistance, data = seed.lm.train)
seed.glm.model <- glm(FirstTeamWin ~ SeedDistance, data = seed.lm.train, family = "binomial")
#training set prediction
seed.lm.train.prediction <- predict(seed.lm.model, seed.lm.train)
seed.glm.train.prediction <- predict(seed.glm.model, seed.lm.train, type = "response")
#log loss function
MultiLogLoss <- function(act, pred){
    eps <- 1e-15
    pred <- pmin(pmax(pred, eps), 1 - eps)
    sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}

MultiLogLoss(seed.lm.train$FirstTeamWin, seed.lm.train.prediction)
MultiLogLoss(seed.lm.train$FirstTeamWin, seed.glm.train.prediction)

#split the id number into the 3 columns we need
seed.lm.submission <- cbind(sample_submission$id, colsplit(sample_submission$id, split="_", names=c("Season", "FirstTeam", "SecondTeam")))

#add seed data
seed.lm.submission <- seed.lm.submission %>%
    left_join(tourney_seeds, by=c("Season", "FirstTeam" = "Team")) %>%
    left_join(tourney_seeds, by=c("Season", "SecondTeam" = "Team"), suffix=c(".FirstTeam", ".SecondTeam"))

#extract numerical part of seed
seed.lm.submission$Seed.FirstTeam <- str_extract(seed.lm.submission$Seed.FirstTeam, "[:digit:]+")
seed.lm.submission$Seed.SecondTeam <- str_extract(seed.lm.submission$Seed.SecondTeam, "[:digit:]+")

#calculate seed distance
seed.lm.submission$Seed.FirstTeam <- as.numeric(seed.lm.submission$Seed.FirstTeam)
seed.lm.submission$Seed.SecondTeam <- as.numeric(seed.lm.submission$Seed.SecondTeam)
seed.lm.submission$SeedDistance <- seed.lm.submission$Seed.SecondTeam - seed.lm.submission$Seed.FirstTeam

#make prediction and write to a csv file
seed.lm.submission.prediction <- predict(seed.lm.model, seed.lm.submission)
seed.lm.submission <- data.frame(id=sample_submission$id, pred=seed.lm.submission.prediction)
write.csv(seed.lm.submission, file="SeedLinearModelSubmission.csv", row.names=FALSE)