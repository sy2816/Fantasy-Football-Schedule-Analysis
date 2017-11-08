#make some mock data
setwd("C:/Fantasy-Football-Schedule-Analysis")
library(dplyr)
library(highcharter)
library(reshape2)
data <- data.frame(
  team = letters[1:12],
  week1 = runif(n = 12, min = 70, max = 130),
  week2 = runif(n = 12, min = 70, max = 130),
  week3 = runif(n = 12, min = 70, max = 130),
  week4 = runif(n = 12, min = 70, max = 130),
  week5 = runif(n = 12, min = 70, max = 130),
  week6 = runif(n = 12, min = 70, max = 130),
  week7 = runif(n = 12, min = 70, max = 130),
  week8 = runif(n = 12, min = 70, max = 130),
  week9 = runif(n = 12, min = 70, max = 130),
  week10 = runif(n = 12, min = 70, max = 130),
  week11 = runif(n = 12, min = 70, max = 130)
)

transactions <- openxlsx::read.xlsx("FF.xlsx", sheet = 2)
transactions$total <- rowSums(transactions[,c(2,3,5)])
transactions <- arrange(transactions, desc(Acquisitions))
# x is player schedule is for, y is vector of all possible teams (including x)
createSchedule <- function(x, y){
  teams <- y
  teams <- as.character(teams[teams != x])
  
  possibleTeams <- teams
  schedule <- data.frame(week = c(1:11), player = NA, result = NA)
  for(i in c(1:11)){
    week <- i
    matchup <- sample(possibleTeams, 1)
    schedule$player[i] <- matchup
    weekNum <- paste0("week", i)
    xScore <- data[,weekNum][data$team == x] #Change week1 to corresponding week in iteration.
    yScore <- data[,weekNum][data$team == matchup]
    if (xScore > yScore){
      result <- "w"
    }else{
      result <- "l"
    }
    schedule$result[i] <- result
    possibleTeams <- possibleTeams[possibleTeams != matchup]
  }
  return(schedule)
}

# create empty vector to add to
iterations <- 10000
wins <- rep(NA, iterations)

# Run 10000 simulations
for (i in c(1:iterations)){
  schedule <- createSchedule("b", data$team)
  numWins <- length(grep(pattern = "w", schedule$result))
  wins[i] <- numWins
}

# Calculate summary statistics
minWins <- min(wins)
maxWins <- max(wins)
meanWins <- round(mean(wins), 1)

# Create frequency dataframe
wins <- sort(wins)
winDat <- data.frame(iteration = c(1:iterations),
                     wins = wins)
winCount <- dplyr::count(winDat, wins)
winCount$percentage <- round(((winCount$n/iterations) * 100), 1)

# Graph
highchart() %>%
  hc_add_series(data = winCount, "column", hcaes(x = wins, y = percentage), name = "Percent Chance")%>%
  hc_xAxis(plotLines = list(list(
    value = meanWins, color = "black", width = 1.5,
    label = list(text = paste0("Average # of Wins - ", meanWins)),
    zIndex = 5
  )),
  title = list(text = "Number of Wins")) %>%
  hc_yAxis(labels = list(format = "{value} %")) %>%
  hc_tooltip(
    pointFormat = "Probability of winning {point.x} games: <b>{point.y}</b><br/>",
    valueSuffix = ' %',
    headerFormat = ""
  ) %>%
  hc_title(text = "Number of wins based on 10,000 simulations")

mtransactions <- melt(data = transactions, measure.vars = "team")

####Transactions graph
highchart() %>%
  hc_xAxis(categories = transactions$team) %>%
  hc_add_series(data = transactions$Trades, type = "column", name = "Trades") %>%
  hc_add_series(data = transactions$Acquisitions, type = "column", name = "Acquisitions") %>%
  hc_add_series(data = transactions$Activate, type = "column", name = "Activations")%>%
  hc_title(text = "Transactions by Type")
  

