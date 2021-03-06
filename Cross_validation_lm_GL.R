################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Model_final.R
# Purpose of script:        Cross-validation of final model GL
# Datafiles used:	          Clean_data_GL_2018-12-14.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-17
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)


#### Set up ####
rm(list = ls()) # empty work space
Data_GL <- read.csv("1_clean_data/Clean_data_GL_2018-12-21.csv",
                    stringsAsFactors=F, header = T)

#### Final model ####
final_model_lm_GL<- lm(sqrt(GL_frac) ~ High_educated_frac + 
                         Mean_income +Non_west + Frac_60plus,data = Data_GL)

summary(final_model_lm_GL) 



#### Make folds ####
K <- 5
index <- rep(1:K, floor(nrow(Data_GL)/K)+1)[1:nrow(Data_GL)]
summary(as.factor(index))
fold.index <- sample(index)

Loss <- function(x, y){
  sum((x-y)^2)/length(x)
}
loss <- numeric(K)

for (k in 1:K){
  training <- Data_GL[fold.index!=k, ]
  validation <- Data_GL[fold.index==k, ]
  training.fit <- final_model_lm_GL
  validation.predict <- predict(training.fit, newdata=validation, type='response')
  loss[k] <- Loss(validation$GL_frac, validation.predict)
}

#average, with weights equal to the number of objects used to calculate the loss at each fold:
mean(loss)
