################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Model_final2.R
# Purpose of script:        Constructing final model & checking assumptions
#                           for party GL
# Datafiles used:	          Clean_data_GL_2018-12-14.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-14
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)
library(lindia)

#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_GL_2018-12-14.csv", 
                 header = T, stringsAsFactors = F)

Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor
row.names(Data) <- Data$Muni
summary(Data)

#### Model1 ####
model1 <- lm(GL ~ Urban_index + High_edu_perc + Mean_income + Perc_60plus +
                Non_west, data = Data)

summary(model1)

# Check assumptions 
par(mfrow = c(2,2))
plot(model1, which = 1)
plot(model1, which = 2)
plot(model1, which = 3)
plot(model1, which = 4) 
abline(h = 0.011, col = "red")



avPlots(model1)
gg_boxcox(model1)

# GL needs a sqrt transformation

#### Model1a ####
# Transformation of response (sqrt)
model1a <- lm(sqrt(GL) ~ Urban_index + High_edu_perc + Mean_income + Perc_60plus +
               Non_west, data = Data)

summary(model1a)

# Check assumptions 
par(mfrow = c(2,2))
plot(model1a, which = 1)
qqPlot(model1a)
plot(model1a, which = 3)
plot(model1a, which = 4)
abline(h = 0.011, col = "red")

# Have closer look at extreme values
Data[c(42, 307,308),]





#### Model2 ####
# We remove Urbanity index 
model2 <- lm(sqrt(GL) ~ High_edu_perc + Mean_income + Perc_60plus + 
                Non_west, data = Data)
summary(model2)
# Compare the two models
anova(model1a, model2)

# Check assumptions 
par(mfrow = c(1,2))
plot(model2, which = 1)
qqPlot(model2)

plot(model2, which = 3)
plot(model2, which = 4)


# Check leverage points again, now numerically 
# 24/369 municipalities have high leverage
levs <- lm.influence(model2)$hat 
cutoff.lev <- 2*4/369 
levs[levs>cutoff.lev]

# Check for outliers
rstudent(model2) [abs(rstudent(model2)) > 2]
outlierTest(model2)





#### Model3 ####
# We remove mean income
model3 <- lm(sqrt(GL) ~ High_edu_perc + Perc_60plus +
               Non_west, data = Data)

summary(model3)

# Check assumptions 
par(mfrow = c(2,2))
plot(model3, which = 1)
qqPlot(model3)
plot(model3, which = 3)
plot(model3, which = 4) # cut off: 4 / (369 - 2 - 1) = 0.011

# Look at extreme values
Data[c(16, 307, 308),]




#### compare all models ####
step(model1a) # model2 best
anova(model1a, model2)
anova(model2, model3) # model3 better than model2
anova(model1a, model2, model3)



#### Conclusion: model2 ####
####################### End script #############################################