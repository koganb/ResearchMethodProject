if(require("sqldf") == FALSE) install.packages("sqldf")
library(sqldf)

if(require("dplyr") == FALSE) install.packages("dplyr")
library(dplyr)

library(mlbench)
library(caret)


source("data_preparation.R")

createGameStats <- function(players, games){
  players.cleaned <- preparePlayersDataSet(players)
  
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

players <- read.csv(file = "data/players.csv", header = TRUE, stringsAsFactors=F)
games <- read.csv(file = "data/games.csv", header = TRUE, stringsAsFactors=F)
games.stats <- createGameStats(players, games)

#now we are moving to the modeling file... all models are there
