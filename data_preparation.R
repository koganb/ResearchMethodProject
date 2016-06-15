
if(require(" tidyr") == FALSE) install.packages(" tidyr", repos = "http://cran.us.r-project.org")
library( tidyr)
if(require("dplyr") == FALSE) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)




#https://rpubs.com/bradleyboehmke/data_wrangling
preparePlayersDataSet <- function(players) {
  #numeric columns
  numeric_cols <- c("MIN","OREB","DREB","REB","AST","STL","BLK","TO","PF","PLUS_MINUS","PTS")
  
  #columns that have XX-XX pattern
  split_cols_list <- list(FG=c("good_shots", "shots"), 
                          X3PT=c("good_3_points", "three_points_shots"), 
                          FT=c("good_FT_points", "FT_shots"))
  splitable_cols <- names(split_cols_list)
  
  
  #remove rows with non numeric values or players which played less than 2 min in a game
  suppressWarnings(players <- players[!is.na(as.numeric(players$MIN)),])
  players <- players[as.numeric(players$MIN)>2,]
  
  
  ##convert the name of the +- column, it didn't wasn't imported as needed
  names(players) <- names(plyr::rename(players, c("X..."="PLUS_MINUS")))
  
  #split data into 2 columns
  for (col in splitable_cols){
    players <- separate_(data = players, col = col, into = c(split_cols_list[[col]]), sep = "-")
  }
  
  #convert to numeric
  for(i in c(numeric_cols,  as.vector(unlist(split_cols_list)))) {
    players[,i] <- as.numeric(players[,i])
  }
  
  return(players)
}


#players <- read.csv(file = "data/players.csv", header = TRUE, stringsAsFactors=F)
#players_conv <- prepareGamesDataSet(players)

