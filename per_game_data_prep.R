if(require("sqldf") == FALSE) install.packages("sqldf")
library(sqldf)

if(require("dplyr") == FALSE) install.packages("dplyr")
library(dplyr)

library(mlbench)
library(caret)

createGameStats <- function(players.path, games.path){
  games.path <- "C:/Users/abrahami/Documents/Private/Uni/BGU/research_methods/analysis_and_models/games.csv"
  players.path <- "C:/Users/abrahami/Documents/Private/Uni/BGU/research_methods/analysis_and_models/players.csv"
  
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
  
  games.stats.query <-  "SELECT g.PLAY_SEASON, p.GAME_ID, p.TEAM,
                            (case when p.Team = g.HOME_TEAM then 1 else 0 end) AS Is_Hosting,
                            (case when ((g.HOME_TEAM_SCORE > g.VISIT_TEAM_SCORE AND p.Team = g.HOME_TEAM)
                                        OR (g.HOME_TEAM_SCORE < g.VISIT_TEAM_SCORE AND p.Team = g.VISIT_TEAM)) then 1 else 0 end) AS Is_Winner,
                            (case when p.Team = g.HOME_TEAM then HOME_TEAM_SCORE - VISIT_TEAM_SCORE else VISIT_TEAM_SCORE - HOME_TEAM_SCORE end) AS game_diff,
                            sum(p.OREB) AS o_rebounds,
                            sum(p.DREB) AS d_rebounds,
                            sum(p.REB) AS rebounds,
                            sum(p.AST) AS assists,
                            sum(p.STL) AS steals,
                            sum(p.BLK) AS blocks,
                            sum(p.[TO]) AS turnovers,
                            sum(p.PF) AS fouls,
                            sum(p.PLUS_MINUS) AS plus_minus,
                            sum(p.PTS) AS points,
                            sum(p.good_shots) AS good_shots,
                            sum(p.shots) AS shots,
                            sum(p.good_3_points) AS good_three_points,
                            sum(p.three_points_shots) AS [three_points_shots],
                            sum(p.good_FT_points) AS good_FT_points,
                            sum(p.FT_shots) AS FT_shots,
                            sum(p.good_shots)/sum(p.shots) AS shots_perc,
                            sum(p.good_3_points)/sum(p.three_points_shots) AS three_shots_perc,
                            sum(p.good_FT_points)/sum(p.FT_shots) AS FT_perc,
                            STDEV(p.PTS) AS points_std
                            FROM [players.cleaned] p
                            INNER JOIN [games] g
                              ON p.GAME_ID = g.GAME_ID
                            GROUP BY g.PLAY_SEASON, p.GAME_ID, p.TEAM"
  games.stats.data <- sqldf(games.stats.query, stringsAsFactors = FALSE)
  return (games.stats.data)
}

evalRegressionFunction <- function(basic.features, added.feature,target, data){
  fml <- as.formula(paste(target,"~",paste(basic.features,collapse="+")))
  basic.model <- lm(formula = fml, data = data)
  
  fml <- as.formula(paste(target,"~",paste(c(basic.features, added.feature), collapse="+")))
  advanced.model <- lm(formula = fml, data = data)
  
  r.square.improvement <- summary(advanced.model)$adj.r.squared - summary(basic.model)$adj.r.squared
  
  # estimate variable importance
  control <- trainControl(method="repeatedcv", number=10, repeats=3)  # prepare training scheme
  model <- train(fml, data = data, method = "glmnet", preProcess = "scale", trControl=control)
  importance <- varImp(model, scale=FALSE)
  plot(importance)
  return (resutls  = list(r.square.improvement = r.square.improvement, feature.importance = importance))
}

####################### Function usage #####################
#functions call:
games.path <- "C:/Users/abrahami/Documents/Private/Uni/BGU/research_methods/analysis_and_models/games.csv"
players.path <- "C:/Users/abrahami/Documents/Private/Uni/BGU/research_methods/analysis_and_models/players.csv"
games.stats <- createGameStats(players.path = players.path, games.path = games.path)

#now we are moving to the modeling file... all models are there
