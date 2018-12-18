################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Floor Komen 
# Script:                   Model_final_GL.R
# Purpose of script:        Constructing final model & checking assumptions
#                           for party GL
# Datafiles used:	          Clean_data_GL_2018-12-06.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-06
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)


#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_GL_2018-12-14.csv", 
                 header = T, stringsAsFactors = F)

# Add binairy variables
Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor
row.names(Data) <- Data$Muni # change rownames to the municipalities
Data$GL_perc <- round(Data$GL * 1000, digits = 0)
Data$Voted_other <- 1000 - Data$GL_perc

#### Model 1 ####
# Start with all explanatory variables

model1 <- glm(cbind(GL_perc,Voted_other)~ Urban_index + High_edu_perc + 
                Mean_income +Non_west + Perc_60plus, 
              family=binomial,data = Data)

Anova(model1, type = "2")
step(model1)

summary(model1) 

avPlots(model1)

# Check assumptions 
par(mfrow = c(1,2))
plot(model1, which = 1, id.n = 5) 

plot(model1, which = 3, id.n = 5) 
plot(model1, which = 4, id.n = 5)
abline(h = 0.011, col = "red")



#### Model 2 ####
# exclude Urban_index

model2 <- glm(cbind(GL_perc,Voted_other)~ High_edu_perc + 
                Mean_income +Non_west + Perc_60plus, 
              family=binomial,data = Data)
summary(model2)

Anova(model2, type = "2")
avPlots(model2)

# Check assumptions 
par(mfrow = c(1,2))
plot(model2, which = 1, id.n = 5) 

plot(model2, which = 3, id.n = 5) 
plot(model2, which = 4, id.n = 5)
abline(h = 0.011, col = "red")

