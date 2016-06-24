if(require("sqldf") == FALSE) install.packages("sqldf", repos = "http://cran.us.r-project.org")
library(sqldf)

if(require("dplyr") == FALSE) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

library(mlbench)
library(caret)


source("data_preparation.R")

createGameStats <- function(players, games){
  players.cleaned <- preparePlayersDataSet(players)
  
  season.averages.query <-  "SELECT g.PLAY_SEASON, p.TEAM, count(distinct p.GAME_ID) AS games_counter,
                              sum(p.OREB)/count(distinct p.GAME_ID) AS o_rebounds_avg,
                              sum(p.DREB)/count(distinct p.GAME_ID) AS d_rebounds_avg,
                              sum(p.REB)/count(distinct p.GAME_ID) AS rebounds_avg,
                              sum(p.AST)/count(distinct p.GAME_ID) AS assists_avg,
                              sum(p.STL)/count(distinct p.GAME_ID) AS steals_avg,
                              sum(p.BLK)/count(distinct p.GAME_ID) AS blocks_avg,
                              sum(p.[TO])/count(distinct p.GAME_ID) AS turnovers_avg,
                              sum(p.PF)/count(distinct p.GAME_ID) AS fouls_avg,
                              sum(p.PLUS_MINUS)/count(distinct p.GAME_ID) AS plus_minus_avg,
                              sum(p.PTS)/count(distinct p.GAME_ID) AS points_avg,
                              sum(p.good_shots)/count(distinct p.GAME_ID) AS good_shots_avg,
                              sum(p.shots)/count(distinct p.GAME_ID) AS shots_avg,
                              sum(p.good_3_points)/count(distinct p.GAME_ID) AS good_three_points_avg,
                              sum(p.three_points_shots)/count(distinct p.GAME_ID) AS three_points_shots_avg,
                              sum(p.good_FT_points)/count(distinct p.GAME_ID) AS good_FT_points_avg,
                              sum(p.FT_shots)/count(distinct p.GAME_ID) AS FT_shots_avg,
                              sum(p.good_shots)/sum(p.shots) AS shots_perc_avg,
                              sum(p.good_3_points)/sum(p.three_points_shots) AS three_shots_perc_avg,
                              sum(p.good_FT_points)/sum(p.FT_shots) AS FT_perc_avg
                              FROM [players.cleaned] p
                              INNER JOIN [games] g
                                ON p.GAME_ID = g.GAME_ID
                              GROUP BY g.PLAY_SEASON, p.TEAM"
  season.averages.data <- sqldf(season.averages.query, stringsAsFactors = FALSE)

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
                            STDEV(p.PTS) AS points_std,
                            STDEV(p.MIN) AS minutes_std,
                            count(distinct NAME) AS Players_counter,
                            
                            sum(p.OREB)/avg(s.o_rebounds_avg) AS o_rebounds_relative,
                            sum(p.DREB)/avg(s.d_rebounds_avg) AS d_rebounds_relative,
                            sum(p.REB)/avg(s.rebounds_avg) AS rebounds_relative,
                            sum(p.AST)/avg(s.assists_avg) AS assists_relative,
                            sum(p.STL)/avg(s.steals_avg) AS steals_relative,
                            sum(p.BLK)/avg(s.blocks_avg) AS blocks_relative,
                            sum(p.[TO])/avg(s.turnovers_avg) AS turnovers_relative,
                            sum(p.PF)/avg(s.fouls_avg) AS fouls_relative,
                            sum(p.PLUS_MINUS)/avg(s.plus_minus_avg) AS plus_minus_relative,
                            sum(p.PTS)/avg(s.points_avg) AS points_relative,
                            sum(p.good_shots)/avg(s.good_shots_avg) AS good_shots_relative,
                            sum(p.shots)/avg(s.shots_avg) AS shots_relative,
                            sum(p.good_3_points)/avg(s.good_three_points_avg) AS good_three_points_relative,
                            sum(p.three_points_shots)/avg(s.three_points_shots_avg) AS three_points_shots_relative,
                            sum(p.good_FT_points)/avg(s.good_FT_points_avg) AS good_FT_points_relative,
                            sum(p.FT_shots)/avg(s.FT_shots_avg) AS FT_shots_relative,
                            (sum(p.good_shots)/sum(p.shots))/avg(s.shots_perc_avg) AS shots_perc_relative,
                            (sum(p.good_3_points)/sum(p.three_points_shots))/avg(s.three_shots_perc_avg) AS three_shots_perc_relative,
                            (sum(p.good_FT_points)/sum(p.FT_shots))/avg(s.FT_perc_avg) AS FT_perc_relative
                            FROM [players.cleaned] p
                            INNER JOIN [games] g
                              ON p.GAME_ID = g.GAME_ID
                            INNER JOIN [season.averages.data] s
                              ON p.TEAM = s.TEAM AND g.PLAY_SEASON = s.PLAY_SEASON
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
