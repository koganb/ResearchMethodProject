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
feature.names <- names(games.stats)[c(7:14, 16:22)]
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

#d. features importance, based regression
library(mlbench)
library(caret)

set.seed(7) # ensure results are repeatable
control <- trainControl(method="repeatedcv", number=10, repeats=3)  # prepare training scheme
feature.names <- names(games.stats)[c(7:14, 16:22)]
model <- train(game_diff~., data = games.stats[,c('game_diff',feature.names)], method = "glmnet", preProcess = "scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

#just changing the method to be somthing differnet
model <- train(game_diff~., data = games.stats[,c('game_diff',feature.names)], method = "rf", preProcess = "scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)


