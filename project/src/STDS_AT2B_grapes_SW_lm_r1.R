library(tidyverse) # We need to load below packages.
library(readxl)
library(magrittr)
library(corrplot)
library(here)
library(mctest)
library(Boruta)
library(MASS)

par("mar")
par(mar=c(1,1,1,1))
# setup random number generator seed
set.seed(10) 
# Modeling Framework
# Read source dataset
yield_by_region <- read.csv(here::here("/project/src/output/yield_by_region.csv"))
# get index of response variable
yield_index = grep("yield", colnames(yield_by_region))
# select subset of data
## only 5 main states
## only attributes from yield dataset
## remove categorical attributes
cols = colnames(yield_by_region)[1:yield_index]
yield_by_region_1 <- yield_by_region %>% 
  filter(state %in% c('NSW', 'WA', 'Tas', 'Vic', 'SA'))%>%
  dplyr::select(cols) %>%
  dplyr::select(-c(code, region, state, lon, lat))

# set train and test dataset
train_pct = 0.7
# identify train dataset size (number of rows)
train_size <- floor(train_pct * nrow(yield_by_region_1))
# use sample function to form a random train dataset
train_n <- sample(seq_len(nrow(yield_by_region_1)), size = train_size)
# generate train dataset
train <- yield_by_region_1[train_n, ]
# test dataset is the supplementary of train dataset
test <- yield_by_region_1[-train_n, ]

# Select and Fit for Variables
run_model <- function(train, test, features, model_selection) {
  # further filter train dataset with feature selections
  train1 <- train %>% 
    #dplyr::select(lon:total_water_used)
    dplyr::select(features)
  # test dataset doesn't change
  test1 <- test
  
  if(model_selection == "lm") {
    fit_select <- lm(data = train1, yield ~ .)
    
  }
  if(model_selection == "glm") {
    fit_select <- glm(data = train1, yield ~ .,
                      # family = inverse.gaussian(link = "log"),
                      # family = inverse.gaussian(link = "1/mu^2"),
                      # family = Gamma(link = "inverse"),
                      family = gaussian(link = "identity"))
  }
  # Stats
  summary(fit_select) 
  # Retain model if vif (Variable Inflation Factors) of each feature is below 5. 
  # vif(fit_select) 
  # Diagnostic Plots
  # resid_panel(fit_select, plots = "all") 
  # Calculate residuals values of model for training data 
  train1$residuals <- residuals(fit_select) 
  # Calculate predicted values of model for training data 
  train1$predicted <- predict(fit_select, train1, type="response") 
  rmse_train1 <- sqrt(mean(train1$residuals ** 2)) 
  # Calculate residuals values of model for training data 
  test1$predicted <- predict(fit_select, test1) 
  #  Calculate residuals values of model for test data 
  test1$residuals <- test1$yield - test1$predicted 
  # Root Mean Squared Error for test data 
  rmse_test1 <- sqrt(mean(test1$residuals ** 2)) 
  # Check RMSEs shouldnt exceed 15% of each other
  rmse_ratio <- rmse_test1/rmse_train1 
  # when this number is greater than 1, it means there are more deviation observed
  # on test set, which indicates the model overfits on train dataset - bad!
  
  res_list <- list("rmse_test"=rmse_test1, "rmse_ratio"=rmse_ratio)
  return (res_list)
}

model_report <- function(model) {
  print(model$rmse_ratio)
  if(model$rmse_ratio > 1) {
    print("rmse_ratio greater than 1, not good fitting.")
  }
  print("model rmse:")
  print(model$rmse_test)
}



# Model 1 - Variable Selection with Boruta Algorithm (multiple variables)
# https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
boruta_output <- Boruta(yield ~ ., data=na.omit(yield_by_region_1), doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

pdf(paste("project/src/plot/grape/", "var_importance.pdf", sep=""))
p <- plot(boruta_output, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_output$ImpHistory),function(i)
  boruta_output$ImpHistory[is.finite(boruta_output$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_output$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_output$ImpHistory), cex.axis = 0.7)
print(p)
dev.off()
# plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

features <- getSelectedAttributes(boruta_output, withTentative = F)
important_features <- c(features, "yield")
m1 <- run_model(train, test, important_features, "lm")
model_report(m1)
for(i in 1:length(features)) {
  print(features[i])
}

