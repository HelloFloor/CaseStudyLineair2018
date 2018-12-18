################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Model_final.R
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
library(xtable)

#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_CDA_2018-12-14.csv", 
                 header = T, stringsAsFactors = F)


# Add binairy variables
Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor
row.names(Data) <- Data$Muni # change rownames to the municipalities
Data$CDA_perc <- round(Data$CDA * 1000, digits = 0)
Data$Voted_other <- 1000 - Data$CDA_perc



#### Model 1 ####
# Start with all explanatory variables
model1 <- glm(cbind(Voted_other, CDA_perc) ~ Urban_index + High_edu_perc + 
                 Mean_income +Non_west + Perc_60plus, 
               family=binomial,data = Data)

xtable(Anova(model1, type = "2"))
step(model1)
xtable(summary(model1)) 

avPlots(model1)



# Check assumptions 
par(mfrow = c(1,2))
plot(model1, which = 1, id.n = 5) 

plot(model1, which = 3, id.n = 5) # var of error not equal, non-linearity, obs 300 & 76 outliers
plot(model1, which = 4, id.n = 5)# cutt off val: 0.011. Everything above cut-off is outlier
abline(h = 0.011, col = "red")



#### Model1a ####
# Add logit link function
model1a <- glm(cbind(CDA_perc, Voted_other) ~ Urban_index + High_edu_perc + 
                Mean_income +Non_west + Perc_60plus, 
              family=binomial(link = "logit"),data = Data)

xtable(summary(model1a))
Anova(model1a, type = "II")
avPlots(model1a)

par(mfrow = c(1,2))
plot(model1a, which = 1, id.n = 5) 
plot(model1a, which = 3, id.n = 5) 
plot(model1a, which = 4, id.n = 5)
abline(h = 0.011, col = "red")
qqPlot(model1a$residuals)

#### Model2 ####
# Remove mean_income
# This variable is significant, however we have three reasons for removing it:
#    1. It has a very small influence on the estimate of the model
#    2. It is correlated with High_edu: corr = 0.56
#    3. It explains a very small portion of the deviance
model2 <- glm(cbind(Voted_other, CDA_perc) ~ Urban_index + High_edu_perc + 
                + Non_west + Perc_60plus, 
               family=binomial(link = "logit"),data = Data)

xtable(summary(model2))
Anova(model2, type = "II")

par(mfrow = c(1,2))
plot(model2, which = 1, id.n = 5) 
plot(model2, which = 3, id.n = 5) 
plot(model2, which = 4, id.n = 5)
abline(h = 0.011, col = "red")
qqPlot(model2$residuals)

avPlots(model2)


#### Model3 ####
# Remove High_edu_perc
# This model turns out the be a worse fit. 
model3 <- glm(cbind(CDA_perc, Voted_other) ~ Urban_index + High_edu_perc + 
                + Non_west + Perc_60plus, 
              family=binomial(link = "logit"),data = Data)

summary(model3)
Anova(model3, type = "II")

avPlots(model3)
