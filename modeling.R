#modeling per seasons average
cor(team.stats$avg_assists, target.table$tot_success)
cor(team.stats$avg_fouls, target.table$tot_success)
cor(team.stats$avg_points, target.table$tot_success)
cor(team.stats$avg_plus_minus, target.table$tot_success)

target.column <- 'rank'
feature.names <- names(team.stats)[4:19]
fml <- as.formula(paste(target.column,"~",paste(feature.names,collapse="+")))

model <- lm(formula = fml, data = cbind(team.stats, rank = target.table$rank))
summary(model)

########### Modeling per game sums ###########################
cor(games.stats[,6:dim(games.stats)[2]])

#a. Simple linear regression
feature.names <- names(games.stats)[c(7:14, 16:26)]
fml <- as.formula(paste('game_diff',"~",paste(feature.names,collapse="+")))
lm.model <- lm(formula = fml, data = games.stats)
summary(lm.model)

#b. Stepwise - NOT WORKING!!
fml <- as.formula(game_diff~1)
starting.model <- lm(formula = fml, data = games.stats[,c('game_diff',feature.names)])
stepwise.model <- step(starting.model, scope=(~ .), steps = 15, direction = "both")

#c. logistic regression
if(require("pROC") == FALSE) install.packages("pROC")
library(pROC)
games.stats$Is_Winner <- as.factor(games.stats$Is_Winner)
feature.names <- names(games.stats)[c(7:14, 16:22)]
fml <- as.formula(paste('Is_Winner',"~",paste(feature.names,collapse="+")))
model <- glm(formula = fml, data = games.stats, family = 'binomial')
summary(model)
#roc curve
roc.obj <- roc(response = games.stats$Is_Winner, predictor = predict(object = model, newdata = games.stats))
roc.plot <- plot.roc(roc.obj)
auc(response = games.stats$Is_Winner, predictor = predict(object = model, newdata = games.stats))

#d. using the evaluation function
evalRegressionFunction(basic.features = c('steals', 'blocks'), added.feature = 'assists', target = 'game_diff', data = games.stats)
