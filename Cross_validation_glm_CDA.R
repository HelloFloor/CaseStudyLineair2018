################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Model_final.R
# Purpose of script:        Cross-validation of final model CDA -- GLM model
# Data_CDAfiles used:	          Clean_Data_CDA_CDA_2018-12-21.csv;
# Data_CDA downloaded:          Data_CDA downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-17
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)


#### Set up ####
rm(list = ls()) # empty work space
Data_CDA <- read.csv("1_clean_data/Clean_Data_CDA_2018-12-21.csv", 
                 header = T, stringsAsFactors = F)


# Add binairy variables
Data_CDA$Non_west <- as.factor(Data_CDA$Non_west) # needs to be recognized as factor




#### Final model ####
glm_CDA_5 <- glm(cbind(CDA_abs, Total_abs - CDA_abs) ~ Urban_index  + 
                   High_educated_frac + Non_west, 
                 family=quasibinomial(link = "logit"), 
                 data = Data_CDA[-c(16, 265),])





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
  training.fit <- glm_CDA_5
  validation.predict <- predict(training.fit, newdata=validation, type='response')
  loss[k] <- Loss(validation$CDA_frac, validation.predict)
}


#average, with weights equal to the number of objects used to calculate 
# the loss at each fold:
mean(loss)




