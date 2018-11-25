setwd("C:/Fantasy-Football-Schedule-Analysis/2018")

scoreList <- readRDS("optimizedData.RDS")

#Aggregate scores
optimalData <- list()

for(i in c(1:length(names(scoreList)))){
  
  team <- names(scoreList[i])
  
  for(w in c(1:11)){
    
    weeklyData <- scoreList[[team]][paste0("week",w)]
    weeklyData <- as.data.frame(weeklyData)
    
    sumPoints <- sum(weeklyData[,5])
    
    optimalData[[team]][paste0("week", w)] <- sumPoints
    
  }
}

#Convert from list to data.frame
optimizedData <- data.frame()
for(i in c(1:length(optimalData))){
  optimizedData <- rbind(optimizedData, as.vector(optimalData[[i]]))
}
colnames(optimizedData) <- paste0("week", c(1:11))
optimizedData$team <- names(optimalData)
optimizedData <- optimizedData[,c(12,c(1:11))] 

#Rename teams
optimizedData$team <- c("Alex",
"Neil",
"Stephen",
"Tommy",
"Leigh",
"Kyle",
"Charlie",
"Kelsey",
"Sam",
"Doug",
"Cooper",
"Erik")

openxlsx::write.xlsx(x = optimizedData, file = "optimizedData.xlsx")


