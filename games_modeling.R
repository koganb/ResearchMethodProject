library(pscl)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(ROCR)



#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      
      
    }
  }
}
getWinPercentage <- function(games.stats, team, season) {
  return (round(
          nrow(games.stats[games.stats$TEAM == team & games.stats$PLAY_SEASON == season & games.stats$Is_Winner == 1,])/
          nrow(games.stats[games.stats$TEAM == team & games.stats$PLAY_SEASON == season,]),
          2)*100)
}

getFeatureInfluence <- function(games.stats, team, season, model.features, features.subset.size = 5) {

  df <- games.stats[games.stats$TEAM == team & games.stats$PLAY_SEASON == season,]
  
  #Avrahami changed this part of the code to be mode statistical oreinted
  
  #model_features <- c("o_rebounds","d_rebounds","assists",
  #                    "steals","blocks","turnovers","fouls",
  #                    "shots_perc","three_shots_perc","FT_perc")
  #glm_formula <- paste0("Is_Winner~",paste(model_features, collapse="+"))
  #df_model <- glm(as.formula(glm_formula),family=binomial(link='logit'),data=df)
  #amova_res <- anova(df_model, test="Chisq")
  #variance_diff <- tail(head(mapply(function(x,y) (y-x)/amova_res$"Resid. Dev"[1]*100, c(amova_res$"Resid. Dev", 0), c(0,amova_res$"Resid. Dev")),-1),-1)
  
  #model_features_var <- as.data.frame(variance_diff, stringsAsFactors = F) 
  #model_features_var$features <- model_features
  #model_features_var$variance_diff<- as.numeric(model_features_var$variance_diff)
  glm.formula <- paste0("Is_Winner~",paste(model.features, collapse="+"))
  stepwise.model <- step(object = glm(as.formula("Is_Winner~1"),family=binomial(link='logit'),data=df), scope = glm.formula, direction = "forward", steps = features.subset.size)
  anova.table <- stepwise.model$anova
  variance.diff <- tail(head(mapply(function(x,y) (y-x)/anova.table$"Resid. Dev"[1]*100, c(anova.table$"Resid. Dev", 0), c(0,anova.table$"Resid. Dev")),-1),-1)
  model.features.var <- as.data.frame(variance.diff, stringsAsFactors = F)
  model.features.var$features <- apply(X = as.data.frame(anova.table$Step),MARGIN = 1, function(x) substring(x,3))[-1]
  model.features.var$variance.diff<- as.numeric(model.features.var$variance.diff)
  return (model.features.var)
}

mostInfluentFeatures <- function(dataframe, features_num) {
  most_influent_features <- head(dataframe[order(-dataframe$variance.diff), , drop = FALSE],features_num)$features
  return (most_influent_features)
}

#returns some evaluation measure about our ability to predict results for a team in a season.
#gets as input the big table of games ststs, the team, the season and the features to use for modeling
#returns the AUC and the best accuracy we got, out of all possible thresholds. Done only on train, without any cross-validation
getPredictionAbility <- function(games.stats, team, season, model.features){
  cur.data <- games.stats[games.stats$TEAM == team & games.stats$PLAY_SEASON == season,]
  glm.formula <- paste0("Is_Winner~",paste(model.features, collapse="+"))
  model <- glm(formula = glm.formula, family = binomial(link='logit'), data = cur.data)
  prob <- predict(object = model, newdata = cur.data, type="response")
  pred <- prediction(prob, cur.data$Is_Winner)
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  accuracy <- (pred@tp[[1]]+pred@tn[[1]]) / (pred@n.pos[[1]] + pred@n.neg[[1]])
  best.accuracy <- max(accuracy)
  cross.validation <- cv.glm(data = cur.data, glmfit = model, K=10)
  return(list(auc = auc, accuracy = best.accuracy, CV.accuracy = 1-cross.validation$delta[1]))
}


model_features <- c("o_rebounds","d_rebounds","assists","steals","blocks","turnovers","fouls","shots_perc","three_shots_perc","FT_perc",
                    "shots", "three_points_shots", "FT_shots", "points_std", "minutes_std", "Players_counter",
                    "o_rebounds_relative", "d_rebounds_relative", "assists_relative", "steals_relative", "blocks_relative", "turnovers_relative",
                    "fouls_relative", "shots_relative", "three_points_shots_relative", "FT_shots_relative",
                    "shots_perc_relative", "three_shots_perc_relative", "FT_perc_relative")
model_seasons <-  c('2011/2012','2012/2013','2013/2014', '2014/2015', '2015/2016')

mostInfluentFeaturesTable <- function (games.stats, team_names, seasons=model_seasons, model.features=model_features, features.subset.size = length(model.features)) {
  most_influent_features_table = data.frame()
  
  for (team_name in team_names) {

    for (season in seasons){
      win_perc <- getWinPercentage(games.stats, team_name, season)
      features_table <- getFeatureInfluence(games.stats = games.stats, 
                                            team = team_name, 
                                            season = season, 
                                            model.features = model.features, 
                                            features.subset.size = 5)
      
      features_table <- as.data.frame(t(features_table),  stringsAsFactors = F)
      features_table <- cbind(features_table, data.frame(x=c( win_perc/100, "win_perc"), stringsAsFactors = F))
      colnames(features_table) <- features_table["features",]
      features_table <- features_table[c(TRUE, FALSE),]
      most_influent_features_table <- plyr::rbind.fill(most_influent_features_table, data.frame(as.list(apply(features_table, 2, as.numeric))))
      
      
    }
  }
  return (most_influent_features_table)
}



percentage_without_na <- function (x) {
  return(length(x[!is.na(x)]) / length(x))
}

# source('per_game_data_prep.R')
# players <- read.csv(file = "data/players.csv", header = TRUE, stringsAsFactors=F)
# games <- read.csv(file = "data/games.csv", header = TRUE, stringsAsFactors=F)
# teams <- read.csv(file = "data/teams.csv", header=TRUE, stringsAsFactors = F)
# games.stats <- createGameStats(players, games)

#most_influent_features_table <- mostInfluentFeaturesTable(games.stats, team_names = teams$PREFIX_2)
#most_influent_features_table_sorted <- most_influent_features_table[order(most_influent_features_table$win_perc),] 



#best_seasons <- tail(most_influent_features_table_sorted, 20)
#worst_seasons <- head(most_influent_features_table_sorted, 20)
#worst_season_features <- t(data.frame(as.list(apply(worst_seasons, 2, percentage_without_na))))
#worst_season_features_ordered <- as.data.frame(worst_season_features[order(-worst_season_features[,1]), ])
#worst_season_features_ordered





