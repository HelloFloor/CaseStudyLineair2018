################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Model_final.R
# Purpose of script:        Constructing final model & checking assumptions
#                           for party CDA -- GLM model
# Datafiles used:	          Clean_data_CDA_2018-12-21.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-21
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)
library(xtable)

#### Set up ####
rm(list = ls()) # empty work space
Data_CDA <- read.csv("1_clean_data/Clean_data_CDA_2018-12-21.csv", 
                 header = T, stringsAsFactors = F)


# Add binairy variables
Data_CDA$Non_west <- as.factor(Data_CDA$Non_west) # needs to be recognized as factor
#row.names(Data_CDA) <- Data_CDA$Muni # change rownames to the municipalities




#### Model 1 ####
# Start with all explanatory variables
# We use quassi binomial because the standerd error is adjusted for overdispersion
# The residual deviance does not change in compare to normal binomial
# AIC is not defined for quasi binomial, therefore step doesn't work
glm_CDA_1 <- glm(cbind(CDA_abs, Total_abs) ~ Urban_index + High_educated_frac + 
                 Mean_income +Non_west + Frac_60plus, 
               family=quasibinomial(link = "logit"), data = Data_CDA)

summary(glm_CDA_1)
Anova(glm_CDA_1, type = "2")


# Check partial regression slope
avPlots(glm_CDA_1)


# Check assumptions 
par(mfrow = c(1,2))
plot(glm_CDA_1, which = 1, id.n = 5) 

plot(glm_CDA_1, which = 3, id.n = 5) # var of error not equal, non-linearity
plot(glm_CDA_1, which = 4, id.n = 5)#
abline(h = 0.011, col = "red")


# Look at outliers numerically
outtest <- outlierTest(glm_CDA_1)
outtest$p



#### glm_CDA_1a ####
# Remove outliers
glm_CDA_1a <- glm(cbind(CDA_abs, Total_abs) ~ Urban_index + High_educated_frac + 
                Mean_income + Non_west + Frac_60plus, 
              family=quasibinomial(link = "logit"),data = Data_CDA[-c(16, 265, 305),])

# Summary and variance
summary(glm_CDA_1a)
Anova(glm_CDA_1a, type = "II")
avPlots(glm_CDA_1a)


# Check assumptions
par(mfrow = c(2,2))
plot(glm_CDA_1a, which = 1, id.n = 5) 
qqPlot(glm_CDA_1a$residuals)
abline(h = 0.011, col = "red")
plot(glm_CDA_1a, which = 4, id.n = 5)


# VIF, collinearity
# GVIF^1/2 above 2 is problematic
vif(glm_CDA_1)

#### glm_CDA_2 ####
# Remove mean_income
glm_CDA_2 <- glm(cbind(CDA_abs, Total_abs) ~ Urban_index + High_educated_frac + 
                + Non_west + Frac_60plus, 
               family=quasibinomial(link = "logit"), Data_CDA[-c(16, 265, 305),])

summary(glm_CDA_2)
Anova(glm_CDA_2, type = "II")

# Assumptions
par(mfrow = c(2,2))
plot(glm_CDA_2, which = 1, id.n = 5) 
qqPlot(glm_CDA_2$residuals)
plot(glm_CDA_2, which = 3, id.n = 5) 
plot(glm_CDA_2, which = 4, id.n = 5)
abline(h = 0.011, col = "red")


avPlots(glm_CDA_2)


#### glm_CDA_3 ####
# Remove Frac_60plus
# This model turns out the be a worse fit. 
glm_CDA_3 <- glm(cbind(CDA_abs, Total_abs) ~ Urban_index + High_educated_frac + 
                + Non_west, 
              family=quasibinomial(link = "logit"), Data_CDA[-c(16, 265, 305),])

summary(glm_CDA_3)
Anova(glm_CDA_3, type = "II")

avPlots(glm_CDA_3)

influencials <- lm.influence(glm_CDA_3)
cutoff <- 2*3/369
influencials$hat[influencials$hat > cutoff]
influencials$sigma


#### glm_CDA_4 ####
# Remove High_educated
# This model turns out the be a worse fit. 
glm_CDA_4 <- glm(cbind(CDA_abs, Total_abs) ~ Urban_index  + Non_west,
                 family=quasibinomial(link = "logit"), 
                 Data_CDA[-c(16,94, 265, 305),])

summary(glm_CDA_4)
Anova(glm_CDA_4, type = "II")

vif(glm_CDA_4)
avPlots(glm_CDA_4)


# Assumptions
par(mfrow = c(2,2))
plot(glm_CDA_4, which = 1, id.n = 5) 
qqPlot(glm_CDA_4$residuals)
plot(glm_CDA_4, which = 3, id.n = 5) 
plot(glm_CDA_4, which = 4, id.n = 5)
abline(h = 0.011, col = "red")

#################### End script ################################################