library(pscl)
library(gridExtra)
library(dplyr)
library(ggplot2)



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
          2))
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

