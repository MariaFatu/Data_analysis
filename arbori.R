library(rsample)
library(tidyverse)
library(dplyr)
library(rpart) # necesar pentru CART Decision Trees
library(rpart.plot)
library(caret)
library(randomForest)
library(ipred)


#model for all positions

#simple tree
modelSimpleTree = rpart(
formula = value_eur ~ .,
  data = train,
  method = "anova",
)

#visualise results
modelSimpleTree
rpart.plot(modelSimpleTree)

#visualise cp
plotcp(modelSimpleTree)
modelSimpleTree$cptable

#predict
predSimpleTree = predict(modelSimpleTree, newdata = test)
RMSE(pred = predSimpleTree, obs = test$value_eur)
#better RMSE than linear regression

#see the accuracy of prediction, compared to real values
plotSimpleTreeAccuracy <- tibble(
  i = 1:4871,
  pred = predSimpleTree[],
  actual = test$value_eur
)

ggplot(plotSimpleTreeAcc, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +
  geom_point(aes(y = actual, color = "blue"))

ggplot(plotSimpleTreeAcc, aes(x=i)) + 
  geom_point(aes(y = pred-actual))

#better RMSE than linear regression



#cautam cele mai bune valori pentru parametrii minsplit si maxdepth
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(5, 20, 1)
)

modelsTree <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = value_eur ~. ,
    data = train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
#xerror will test the trees and if overfitting exist the error will grow
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}

mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  
mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula = value_eur ~ .,
  data = train,
  method = "anova",
  control = list(minsplit = 11, maxdepth = 20, cp = 0.01 )
)

predOptimalTree <- predict(optimal_tree, newdata = test)
RMSE(pred = predOptimalTree, obs = test$value_eur)
optimal_tree
rpart.plot(optimal_tree)

#bagging

set.seed(123)

#coob, out-of-bag, is for validation against out-of-bag instances
bagged_tree <- bagging(
  formula = value_eur ~ .,
  data = train, 
  coob = TRUE
)

predBaggedTree <- predict(bagged_tree, test)
RMSE(predBaggedTree, test$value_eur)


plotBaggedTreeAccuracy <- tibble(
  i = 1:4871,
  pred = predBaggedTree[],
  actual = test$value_eur
)
ggplot(plotBaggedTreeAccuracy, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +
  geom_point(aes(y = actual, color = "blue"))

ggplot(plotBaggedTreeAccuracy, aes(x=i)) + 
  geom_point(aes(y = pred-actual))



#assess 10-50 bagged trees
ntree <- 10:50
rmse <- vector(mode = "numeric", length = length(ntree))
for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging(
    formula = value_eur ~ .,
    data = train,
    coob = TRUE,
    nbagg = ntree[i]
  )
  rmse[i] = model$err
}

plot(ntree, rmse, type ="l", lwd=2)

#best at 15
abline(v=15, col = "red", lty="dashed")

set.seed(123)
optimal_bagged <- bagging(
  formula = value_eur ~ .,
  data = train, 
  coob = TRUE,
  nbagg = 15
)

#improvement
optimal_bagged

predOptimalBagged <- predict(optimal_bagged, test)
RMSE(predOptimalBagged, test$value_eur)


plotOptimalBaggedAccuracy <- tibble(
  i = 1:4871,
  pred = predOptimalBagged[],
  actual = test$value_eur
)
ggplot(plotOptimalBaggedAccuracy, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +
  geom_point(aes(y = actual, color = "blue"))

ggplot(plotOptimalBaggedAccuracy, aes(x=i)) + 
  geom_point(aes(y = pred-actual))



#Bagging with CARET
fitControl <- trainControl(
  method = "cv",
  number = 10
)

#fitcontrol, before creating bags, do cv and on 9 folds do bagging and test on 10th
#importance to show the important variables over all trees created;
CVBagged <- train(
  value_eur ~.,
  data = train,
  method = "treebag",
  trControl = fitControl,
  importance = TRUE
)
CVBagged
plot(varImp(CVBagged), 20)

predCVBagged <- predict(CVBagged, test)
RMSE(predCVBagged, test$value_eur)

plo

plotCVBaggedAccuracy <- tibble(
  i = 1:4871,
  pred = predCVBagged[],
  actual = test$value_eur
)
ggplot(plotCVBagged, aes(x=i)) +
  geom_point(aes(y = pred, color = "red")) +
  geom_point(aes(y = actual, color = "blue"))

ggplot(plotCVBagged, aes(x=i)) + 
  geom_point(aes(y = pred-actual))


#random forests

set.seed(123)

simpleRandomForest <- randomForest(
  formula = value_eur ~ .,
  data = train
)
simpleRandomForest
plot(simpleRandomForest)
simpleRandomForest$mse
which.min(simpleRandomForest$mse)
sqrt(simpleRandomForest$mse[which.min(simpleRandomForest$mse)])

#more trees cause less variables

#tuning
features <- setdiff(names(train), "value_eur")
set.seed(123)
m2_rf_tunned <- tuneRF(
  x = train[features],
  y = train$value_eur,
  ntreeTry = 500,
  mtryStart = 5,  #starts with 5 variables and increase by 1.5
  stepFactor = 1.5,
  improve = 0.01, #stop when improvement is less that 1%
  trace = FALSE #
)

m2_rf_tunned
plot(m2_rf_tunned[,1], m2_rf_tunned[,2])
sqrt(m2_rf_tunned[which.min(m2_rf_tunned[,2]),2])

