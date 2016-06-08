if(require("sqldf") == FALSE) install.packages("sqldf")
library(sqldf)

if(require("dplyr") == FALSE) install.packages("dplyr")
library(dplyr)

################################## games - creating the y features
createTarget <- function(path){
  games <- read.csv(file = path, header = TRUE)
  home.games.query <- "SELECT PLAY_SEASON, HOME_TEAM AS TEAM, count(1) AS home_games,
                      sum(case when HOME_TEAM_SCORE > VISIT_TEAM_SCORE then 1 else 0 end) AS home_wins,
                      AVG(HOME_TEAM_SCORE-VISIT_TEAM_SCORE) avg_diff
                      FROM games
                      GROUP BY PLAY_SEASON, HOME_TEAM
                      ORDER BY PLAY_SEASON, TEAM"
  home.games.data <- sqldf(home.games.query,stringsAsFactors = FALSE)
  
  away.games.query <- "SELECT PLAY_SEASON, VISIT_TEAM AS TEAM, count(1) AS away_games,
                      sum(case when HOME_TEAM_SCORE < VISIT_TEAM_SCORE then 1 else 0 end) AS away_wins,
                      AVG(VISIT_TEAM_SCORE-HOME_TEAM_SCORE) avg_diff
                      FROM games
                      GROUP BY PLAY_SEASON, VISIT_TEAM
                      ORDER BY PLAY_SEASON, TEAM"
  away.games.data <- sqldf(away.games.query,stringsAsFactors = FALSE)
  
  games.data <- data.frame(PLAY_SEASON = home.games.data$PLAY_SEASON, TEAM = home.games.data$TEAM,
                           home_success = home.games.data$home_wins/home.games.data$home_games,
                           away_success = away.games.data$away_wins/away.games.data$away_games,
                           tot_success = (home.games.data$home_wins+away.games.data$away_wins)/(home.games.data$home_games + away.games.data$away_games),
                           home_avg_diff = home.games.data$avg_diff,
                           away_avg_diff = away.games.data$avg_diff,
                           tot_avg_diff = (home.games.data$avg_diff + away.games.data$avg_diff)/2)
  
  with.rank <- sorted <- games.data %>% 
    arrange(-tot_success) %>%
    group_by(PLAY_SEASON) %>%
    mutate(rank=row_number())
  
  with.rank <- as.data.frame(with.rank)
  games.data <- with.rank[order(with.rank$PLAY_SEASON, decreasing = FALSE),]
  row.names(games.data) <- NULL
  return (games.data)
}


#################### players data - creating the players stats ##############################
createPlayersStats <- function(players.path, games.path){
  
  #loading the two relevant talbes
  players <- read.csv(file = players.path,header = TRUE)
  games <- read.csv(file = games.path, header = TRUE)
  
  ##convert the name of the +- column, it didn't wasn't imported as needed
  names(players) <- c(names(players)[1:17], "PLUS_MINUS", "PTS")
  ##cleaning cases where players didn't play the whole game
  players.cleaned <- players[!is.na(as.numeric(as.character(players$MIN))),]
  
  ##cleaning cases where players played less than 2 min in a game
  players.cleaned <- players.cleaned[which(as.numeric(as.character(players.cleaned$MIN)) >2),]
  
  
  #preparing all features for modeling - starting with the simple ones (just convert to numeric)
  players.cleaned$MIN <- as.numeric(as.character(players.cleaned$MIN))
  players.cleaned$OREB <- as.numeric(as.character(players.cleaned$OREB))
  players.cleaned$DREB <- as.numeric(as.character(players.cleaned$DREB))
  players.cleaned$REB <- as.numeric(as.character(players.cleaned$REB))
  players.cleaned$AST <- as.numeric(as.character(players.cleaned$AST))
  players.cleaned$STL <- as.numeric(as.character(players.cleaned$STL))
  players.cleaned$BLK <- as.numeric(as.character(players.cleaned$BLK))
  players.cleaned$TO <- as.numeric(as.character(players.cleaned$TO))
  players.cleaned$PF <- as.numeric(as.character(players.cleaned$PF))
  players.cleaned$PLUS_MINUS <- as.numeric(as.character(players.cleaned$PLUS_MINUS))
  players.cleaned$PTS <- as.numeric(as.character(players.cleaned$PTS))
  
  #now the ones we have to convert to meaningful ones
  FG.splitted <- unlist(strsplit(as.character(players.cleaned$FG), split = "-"))
  players.cleaned$good_shots <- as.numeric(FG.splitted[c(TRUE, FALSE)])
  players.cleaned$shots <- as.numeric(FG.splitted[c(FALSE, TRUE)])
  
  X3PT.splitted <- unlist(strsplit(as.character(players.cleaned$X3PT), split = "-"))
  players.cleaned$good_3_points <- as.numeric(X3PT.splitted[c(TRUE, FALSE)])
  players.cleaned$three_points_shots <- as.numeric(X3PT.splitted[c(FALSE, TRUE)])
  
  FT.splitted <- unlist(strsplit(as.character(players.cleaned$FT), split = "-"))
  players.cleaned$good_FT_points <- as.numeric(FT.splitted[c(TRUE, FALSE)])
  players.cleaned$FT_shots <- as.numeric(FT.splitted[c(FALSE, TRUE)])
  
  #only a small subsets of statistics per player - we can make much more if wanted...
  players.stats.query <-  "SELECT g.PLAY_SEASON, p.TEAM, p.NAME,count(1) AS games, avg(p.MIN) AS avg_minutes,
                           AVG(p.PTS) AS avg_points, STDEV(p.PTS) AS std_points,
                           AVG(p.REB) AS avg_rebounds, STDEV(p.REB) AS std_reb,
                           AVG(p.AST) AS avg_assists, STDEV(p.AST) AS std_assists
                           FROM [players.cleaned] p
                           INNER JOIN [games] g
                             ON p.GAME_ID = g.GAME_ID
                           GROUP BY g.PLAY_SEASON, p.TEAM, p.NAME"
  players.stats.data <- sqldf(players.stats.query,stringsAsFactors = FALSE)
  return (players.stats.data)
}


#################### teams data - creating the teams stats ##############################
createTeamsStats <- function(players.path, games.path){
  
  #loading the two relevant talbes
  players <- read.csv(file = players.path,header = TRUE)
  games <- read.csv(file = games.path, header = TRUE)
  
  ##convert the name of the +- column, it didn't wasn't imported as needed
  names(players) <- c(names(players)[1:17], "PLUS_MINUS", "PTS")
  ##cleaning cases where players didn't play the whole game
  players.cleaned <- players[!is.na(as.numeric(as.character(players$MIN))),]
  
  ##cleaning cases where players played less than 2 min in a game
  players.cleaned <- players.cleaned[which(as.numeric(as.character(players.cleaned$MIN)) >2),]
  
  
  #preparing all features for modeling - starting with the simple ones (just convert to numeric)
  players.cleaned$MIN <- as.numeric(as.character(players.cleaned$MIN))
  players.cleaned$OREB <- as.numeric(as.character(players.cleaned$OREB))
  players.cleaned$DREB <- as.numeric(as.character(players.cleaned$DREB))
  players.cleaned$REB <- as.numeric(as.character(players.cleaned$REB))
  players.cleaned$AST <- as.numeric(as.character(players.cleaned$AST))
  players.cleaned$STL <- as.numeric(as.character(players.cleaned$STL))
  players.cleaned$BLK <- as.numeric(as.character(players.cleaned$BLK))
  players.cleaned$TO <- as.numeric(as.character(players.cleaned$TO))
  players.cleaned$PF <- as.numeric(as.character(players.cleaned$PF))
  players.cleaned$PLUS_MINUS <- as.numeric(as.character(players.cleaned$PLUS_MINUS))
  players.cleaned$PTS <- as.numeric(as.character(players.cleaned$PTS))
  
  #now the ones we have to convert to meaningful ones
  FG.splitted <- unlist(strsplit(as.character(players.cleaned$FG), split = "-"))
  players.cleaned$good_shots <- as.numeric(FG.splitted[c(TRUE, FALSE)])
  players.cleaned$shots <- as.numeric(FG.splitted[c(FALSE, TRUE)])
  
  X3PT.splitted <- unlist(strsplit(as.character(players.cleaned$X3PT), split = "-"))
  players.cleaned$good_3_points <- as.numeric(X3PT.splitted[c(TRUE, FALSE)])
  players.cleaned$three_points_shots <- as.numeric(X3PT.splitted[c(FALSE, TRUE)])
  
  FT.splitted <- unlist(strsplit(as.character(players.cleaned$FT), split = "-"))
  players.cleaned$good_FT_points <- as.numeric(FT.splitted[c(TRUE, FALSE)])
  players.cleaned$FT_shots <- as.numeric(FT.splitted[c(FALSE, TRUE)])
  
  teams.stats.query <-  "SELECT g.PLAY_SEASON, p.TEAM, count(distinct p.GAME_ID) AS games,
                          sum(p.OREB)/count(distinct p.GAME_ID) AS avg_o_rebounds,
                          sum(p.DREB)/count(distinct p.GAME_ID) AS avg_d_rebounds,
                          sum(p.REB)/count(distinct p.GAME_ID) AS avg_rebounds,
                          sum(p.AST)/count(distinct p.GAME_ID) AS avg_assists,
                          sum(p.STL)/count(distinct p.GAME_ID) AS avg_steals,
                          sum(p.BLK)/count(distinct p.GAME_ID) AS avg_blocks,
                          sum(p.[TO])/count(distinct p.GAME_ID) AS avg_turnovers,
                          sum(p.PF)/count(distinct p.GAME_ID) AS avg_fouls,
                          sum(p.PLUS_MINUS)/count(distinct p.GAME_ID) AS avg_plus_minus,
                          sum(p.PTS)/count(distinct p.GAME_ID) AS avg_points,
                          sum(p.good_shots)/count(distinct p.GAME_ID) AS avg_good_shots,
                          sum(p.shots)/count(distinct p.GAME_ID) AS avg_shots,
                          sum(p.good_3_points)/count(distinct p.GAME_ID) AS avg_good_3_points,
                          sum(p.three_points_shots)/count(distinct p.GAME_ID) AS avg_3_points_shots,
                          sum(p.good_FT_points)/count(distinct p.GAME_ID) AS avg_good_FT_points,
                          sum(p.FT_shots)/count(distinct p.GAME_ID) AS avg_FT_shots
                          FROM [players.cleaned] p
                          INNER JOIN [games] g
                            ON p.GAME_ID = g.GAME_ID
                          GROUP BY g.PLAY_SEASON, p.TEAM"
  teams.stats.data <- sqldf(teams.stats.query, stringsAsFactors = FALSE)
  return (teams.stats.data)
}
####################### Functions usage #####################
#functions call:
games.path <- "C:/Users/abrahami/Documents/Private/Uni/BGU/research_methods/analysis_and_models/games.csv"
players.path <- "C:/Users/abrahami/Documents/Private/Uni/BGU/research_methods/analysis_and_models/players.csv"
target.table <- createTarget(path = games.path)
players.stats <- createPlayersStats(players.path = players.path, games.path = games.path)
team.stats <- createTeamsStats(players.path = players.path, games.path = games.path)

head(target.table)
head(players.stats)
head(team.stats)

#now we are moving to the modeling file... all models are there

