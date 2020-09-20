# load packages, clear environment ----
library(tidyverse)
library(here)
library(janitor)
library(dplyr)
library(ggplot2)
library(glmnet)
library(ROCR)

remove(list = ls())

# read data ----
yield_by_region <- readRDS(here("project/src/output/yield_by_region.rds"))

# view data summaries ----
glimpse(yield_by_region)
summary(yield_by_region)
str(yield_by_region)

# set train and test ----
train_size <- floor(0.7 * nrow(repurch))

set.seed(10) 
train_n <- sample(seq_len(nrow(repurch)), size = train_size)

train <- repurch[train_n, ]
test <- repurch[-train_n, ]

# check sizes
nrow(repurch) == nrow(train) + nrow(test)

# check target ratio is consistent
repurch %>% 
  count(Target ,name = "percent_T") %>% 
  mutate(percent_T = percent_T / nrow(repurch))

train %>% 
  count(Target ,name = "percent_T") %>% 
  mutate(percent_T = percent_T / nrow(train))

test %>% 
  count(Target ,name = "percent_T") %>% 
  mutate(percent_T = percent_T / nrow(test))

# create matrix of features and vector of target
train_feat <- model.matrix(~ ., train[,-1:-2])  # create matrix of features and remove Target and ID
test_feat <- model.matrix(~ ., test[,-1:-2])
train_targ <- train$Target  # target vector
test_targ <- test$Target

# lasso regression - found to produce better f1 than ridge
cv.fit_lasso = cv.glmnet(train_feat, train_targ, family = 'binomial', alpha = 1, type.measure = "auc", nfolds = 5)

plot(cv.fit_lasso)
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min)

# prediction - against test and train sets (for ROC)
prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx = test_feat, type = "class", s = cv.fit_lasso$lambda.min)  #min produces better f1 score
probability_train = predict(cv.fit_lasso$glmnet.fit, newx = train_feat, type = "response", s = cv.fit_lasso$lambda.min)
probability_test = predict(cv.fit_lasso$glmnet.fit, newx = test_feat, type = "response", s = cv.fit_lasso$lambda.min)

pred <- rep(0, length(probability_test))
pred[probability_test > 0.20] = 1        # produces the same result as the type=class method, default prediction threshold is 0.5  

#confusion matrix - evaluate test
lasso_confusion <- table(predicted = pred, actual=test_targ)
lasso_confusion
lasso_precision <- lasso_confusion[2,2]/(lasso_confusion[2,2]+lasso_confusion[2,1])
round(lasso_precision, digits = 2)
lasso_recall <- lasso_confusion[2,2]/(lasso_confusion[2,2]+lasso_confusion[1,2])
round(lasso_recall, digits = 2)
lasso_f1 <- 2*(lasso_precision*lasso_recall/(lasso_precision+lasso_recall))
round(lasso_f1, digits = 2)

# ROC
train_pred <- prediction(probability_train, train_targ)
test_pred <- prediction(probability_test, test_targ)
train_perf <- performance(train_pred, "tpr", "fpr")
test_perf <- performance(test_pred, "tpr", "fpr")

plot(train_perf,col="red") # plot ROC curve
plot(test_perf, add = TRUE, col="blue")
lines(c(0,1),c(0,1),col = "gray", lty = 4 )
legend('bottomright','groups',c("Train","Test"), lty = c(1,1), col=c('red', 'blue'),ncol=1,bty ="n")

# AUC
train_auc <- unlist(slot(performance(train_pred, "auc"), "y.values"))
round(train_auc, digits = 2)
test_auc <- unlist(slot(performance(test_pred, "auc"), "y.values"))
round(test_auc, digits = 2)
