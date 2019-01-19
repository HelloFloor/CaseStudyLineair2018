################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Model_final.R
# Purpose of script:        Cross-validation of final model CDA
# Datafiles used:	          Clean_data_CDA_2018-12-14.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-17
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)


#### Set up ####
rm(list = ls()) # empty work space
Data_CDA <- read.csv("1_clean_data/Clean_data_CDA_2018-12-21.csv",
                     stringsAsFactors=F, header = T)


#### Final model ####
final_model_lm_CDA <- lm(log(CDA_frac) ~  Urban_index+ Mean_income+
                  Non_west + Frac_60plus,data = Data_CDA[-16,])


summary(final_model_lm_CDA) 



#### Make folds ####
K <- 5
index <- rep(1:K, floor(nrow(Data_CDA)/K)+1)[1:nrow(Data_CDA)]
summary(as.factor(index))
fold.index <- sample(index)

Loss <- function(x, y){
  sum((x-y)^2)/length(x)
}
loss <- numeric(K)

for (k in 1:K){
  training <- Data_CDA[fold.index!=k, ]
  validation <- Data_CDA[fold.index==k, ]
  training.fit <- final_model_lm_CDA
  validation.predict <- predict(training.fit, newdata=validation, type='response')
  loss[k] <- Loss(log(validation$CDA_frac), validation.predict)
}

#average, with weights equal to the number of objects used to calculate the loss at each fold:
mean(loss)

mean(final_model_lm_CDA$residuals^2)

K <- 5
index <- rep(1:K, floor(nrow(Data_CDA)/K)+1)[1:nrow(Data_CDA)]
fold.index <- sample(index)

Loss <- function(x, y){
  sum((x-y)^2)/length(x)
}
loss2 <- numeric(K)

for (k in 1:K){
  training <- Data_CDA[fold.index!=k, ]
  validation <- Data_CDA[fold.index==k, ]
  training.fit <- lm_CDA_2
  validation.predict <- predict(training.fit, newdata=validation, type='response')
  loss2[k] <- Loss(log(validation$CDA_frac), validation.predict)
}
mean(loss2)
mean(loss)
