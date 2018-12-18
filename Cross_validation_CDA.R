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
Data <- read.csv("1_clean_data/Cleandata_CDA_2018-12-14.csv", 
                 header = T, stringsAsFactors = F)


# Add binairy variables
Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor
row.names(Data) <- Data$Muni # change rownames to the municipalities
Data$CDA_perc <- round(Data$CDA * 1000, digits = 0)
Data$Voted_other <- 1000 - Data$CDA_perc



#### Final model ####
final_model <- glm(cbind(CDA_perc, Voted_other) ~ Urban_index + High_edu_perc + 
                 +Non_west + Perc_60plus, 
              family=binomial,data = Data)


summary(final_model) 



#### Make folds ####
K <- 10
index <- rep(1:K, floor(nrow(Data)/K)+1)[1:nrow(Data)]
summary(as.factor(index))
fold.index <- sample(index)

Loss <- function(x, y){
  sum((x-y)^2)/length(x)
}
loss <- numeric(K)

for (k in 1:K){
  training <- Data[fold.index!=k, ]
  validation <- Data[fold.index==k, ]
  training.fit <- final_model
  validation.predict <- predict(training.fit, newdata=validation, type='response')
  loss[k] <- Loss(validation$CDA, validation.predict)
}

#average, with weights equal to the number of objects used to calculate the loss at each fold:
mean(loss)




