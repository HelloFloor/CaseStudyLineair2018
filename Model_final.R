################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   First_models.R
# Purpose of script:        Constructing final model & checking assumptions
#                           for party CDA
# Datafiles used:	          Clean_data_CDA_2018-12-06.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-06
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)


#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_CDA_2018-12-06.csv", 
                 header = T, stringsAsFactors = F)

Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor


#### Model 1 ####
# Start with all explanatory variables
model1 <- lm(CDA ~ Urban_index + High_edu_perc + Mean_income + Non_west,
             data = Data)

summary(model1)

# Check assumptions 
par(mfrow = c(2,2))
plot(model1, which = 1)
plot(model1, which = 2)
plot(model1, which = 3)
plot(model1, which = 4) 

# Hmmm, obs 300 (Tubbergen) must be an outlier!
# Same for obs 76: (Dinkelland)
# Both are next to the German border, Those damn Germans!
# Please investigate Lotte! :P

# Added variable plots 
# Used to investigate joint influence (see meeting 8)
avPlots(model1) 



#### Model 1a ####
# Check model1 when obs 300 and 76 are removed
model1a <- lm(CDA ~ Urban_index + High_edu_perc + Mean_income +
                Non_west, data = Data[-c(76,300),])



# Check assumptions 
par(mfrow = c(2,2))
plot(model1a, which = 1)
plot(model1a, which = 2)
plot(model1a, which = 3)
plot(model1a, which = 4) 

avPlots(model1a)



#### Model 2 ####
# Remove mean income
model2 <- lm(CDA ~ Urban_index + High_edu_perc + Non_west,
             data = Data)

summary(model2)

# Check assumptions 
par(mfrow = c(2,2))
plot(model2, which = 1)
plot(model2, which = 2)
plot(model2, which = 3)
plot(model2, which = 4)
