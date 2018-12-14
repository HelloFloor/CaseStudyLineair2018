################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   First_models.R
# Purpose of script:        Constructing final model & checking assumptions
#                           for party CDA
# Datafiles used:	          Clean_data_2018-12-06.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-06
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)


#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_all_variables_2018-12-06.csv", 
                 header = T, stringsAsFactors = F)

Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor


#### Model1 ####
model1 <- lm(GL ~ Urban_index + High_edu_perc + Mean_income +
                Non_west, data = Data)

summary(model1)
summary(Data$PvdA)
summary(Data$GL)
# Check assumptions 
par(mfrow = c(2,2))
plot(model1, which = 1)
plot(model1, which = 2)
plot(model1, which = 3)
plot(model1, which = 4) 

avPlots(model1)
