#Seed Benchmark
#Ver 0.2
require("data.table")

tourneySeeds <- fread("TourneySeeds.csv")
sampleSubmission <- fread("sample_submission.csv")

#Define a function that extracts the seeds and divisions separately
getSeedDivision <- function(seedsInfo){
  #Seed & Division 
  #THis function gets the seed and division of a team in a given season
  #Input class == "numeric" corresponding to the season of the tournament and the team unique ID
  #Returns class == "character" corresponding to the seed in that season and the division assigned in the tourney
  #seedsInfo <- tourneySeeds[1] #here for debugging
  
  seasonFromData <- seedsInfo[["Season"]]
  seedAndDivision <- seedsInfo[["Seed"]]
  teamFromData <- seedsInfo[["Team"]]
  
  seedTeam <- gsub(pattern = "[A-Z+a-z]", replacement = "", x = seedAndDivision)
  divisionTeam <- gsub(pattern = "[0-9]", replacement = "", x = seedAndDivision)
  #clean the extra letters
  divisionTeam <- gsub(pattern = "[a-z]", replacement = "", x = divisionTeam)  
  
  return(c(seasonFromData, teamFromData, seedTeam, divisionTeam))
}

#Seasons to be tested
seasons2Test <- seq(2013, 2016)

#Seeds and divisions of teams as table (simplified)---------
seedsAndDivisionsMatrix <- t(apply(tourneySeeds[Season %in% seasons2Test], 1, getSeedDivision))

print("Seeds and divisions extracted")

#Matches Information Extraction---------
matches2Predict <- lapply(sampleSubmission$id, function(submissionIds){
  #submissionIds <- sampleSubmission$id[1]
  matchesInfo <- strsplit(submissionIds, "_")[[1]]
  return(as.numeric(matchesInfo))
})
matches2PredictDt <- as.data.table(do.call(rbind, matches2Predict))
setnames(matches2PredictDt, names(matches2PredictDt), c("Season", "Team1", "Team2"))

seedPredictions <- apply(matches2PredictDt, 1, function(matchInformation){
  #matchInformation <- matches2PredictDt[2] #here for debugging
  season <- matchInformation[["Season"]]
  team1 <- matchInformation[["Team1"]]
  team2 <- matchInformation[["Team2"]]
  
  #Seeds table search 
  seasonMatrix <- seedsAndDivisionsMatrix[seedsAndDivisionsMatrix[, 1] == season, -1]
  seedTeam1 <- as.numeric(seasonMatrix[seasonMatrix[, 1] == as.character(team1), 2])
  seedTeam2 <- as.numeric(seasonMatrix[seasonMatrix[, 1] == as.character(team2), 2])
  
  seedBasedBenchmarkPrediction <- 0.5 + (seedTeam2[1] - seedTeam1[1]) * 0.03   
  return(seedBasedBenchmarkPrediction)
})

#Write a .csv file with results
sampleSubmission$pred <- seedPredictions
write.csv(sampleSubmission, "SeedBenchmark.csv", row.names = FALSE)