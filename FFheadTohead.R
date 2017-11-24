library(readxl)
FF <- read_excel("C:/Users/Wagner_N/Desktop/FF.xlsx")

# Create a list to store the name and scores for each week
team <- list(name = NULL, week2= NULL, week3= NULL, week4= NULL, week5= NULL, week6= NULL, week7= NULL, week8= NULL, week9= NULL,
          week10= NULL, week11= NULL)
# This list will contain each team list (contains name and results)
schedules <- list()

# Cry myself to sleep for not having a python dictionary

# Fill out all of the lists
for(i in seq(dim(FF)[2]) ){
  team$name <- FF[i,1]
  team$week1 <- FF[i,2]
  team$week2 <- FF[i,3]
  team$week3 <- FF[i,4]
  team$week4 <- FF[i,5]
  team$week5 <- FF[i,6]
  team$week6 <- FF[i,7]
  team$week7 <- FF[i,8]
  team$week8 <- FF[i,9]
  team$week9 <- FF[i,10]
  team$week10 <- FF[i,11]
  team$week11 <- FF[i,12]
  #Add this shit to the vector of scehdules
  schedules[[i]] <- team
}

# This function takes two team objects determines the win loss record for the "home" team
makeRecord <- function(home, away){
  wins <- 0
  losses <- 0
  
  # this loops through each element name in the team object
  for(element in names(home)){
    if(element != 'name'){
      #Compare the wins and loses between home and away
      if( home[[element]] >= away[[element]]){
        wins <- wins + 1
      } else {
        losses <- losses +  1
      }
    }
  }
  results <- paste(wins, "-",losses)
  return(results)
}

#Loop a bunch to make a matrix of win loss records
df <- FF
names <- as.vector(FF$team)
colnames(df)[1:12] <- names
rownames(df)[1:12] <- names

# Loop through each row
for( rowNumb in seq( dim(df)[1]) ){
  # Loop through each column
  for( colNumb in seq( dim(df)[2]) ){
    df[rowNumb, colNumb] <- makeRecord(schedules[[rowNumb]], schedules[[colNumb]])
  }
}


