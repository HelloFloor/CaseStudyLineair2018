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
library(tidyverse)
library(MASS)
library(lindia)

#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_CDA_2018-12-14.csv", 
                 header = T, stringsAsFactors = F)


Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor
row.names(Data) <- Data$Muni




#### Model 1 ####
# Start with all explanatory variables
model1 <- lm(CDA ~ Urban_index + High_edu_perc + Mean_income + Non_west + 
               Perc_60plus, data = Data)

summary(model1) # mean income has no significant effect

# Check assumptions 
par(mfrow = c(2,2))
plot(model1, which = 1, id.n = 5) # we see heterogenitiy of variance
qqPlot(model1) # right tail has few  extreme values (non normality)
plot(model1, which = 3, id.n = 5) # var of error not equal, non-linearity, obs 300 & 76 outliers
plot(model1, which = 4, id.n = 5)# cutt off val: 0.011
abline(h = 0.011, col = "red")



# Residuals vs fitted values explanation:
#  1 The residuals "bounce randomly" around the 0 line. This suggests that the assumption
#    that the relationship is linear is reasonable.
#  2. The residuals roughly form a "horizontal band" around the 0 line. This suggests 
#     that the variances of the error terms are equal.
#  3. No one residual "stands out" from the basic random pattern of residuals. This 
#     suggests that there are no outliers.


#### Diff influential & outlier ####
# An outlier is a data point that diverges from an overall pattern in a sample. An outlier has a large residual (the distance between the predicted value () and the observed value (y)). Outliers lower the significance of the fit of a statistical model because they do not coincide with the model's prediction. An influential point is any point that has a large effect on the slope of a regression line fitting the data. They are generally extreme values. The process to identify an influential point begins by removing the suspected influential point from the data set. If this removal significantly changes the slope of the regression line, then the point is considered an influential point.



# CoxBox, because we see heterogenity of variance
# The Boxcox gives suggestions for transformation of the response
# The 95% Confidence interval is located around the 0. Meaning: log transformation!
gg_boxcox(model1)



# Added variable plots 
# Used to investigate joint influence (see meeting 8)
# All already found out that our X variables are correlated. 
# These added variable plots show the simple linear regression 
# between one X variable and another X variable/respons conditional on the other
# variables. The estimates of te Xs suffer from omitted-variable bias
# Therefore, normal simple regressions are not sufficient
# You can now look at the relationship between Y and X2 once all other predictors have been accounted for. So for example, the slope you can see in each plot now reflects the partial regression coefficients from your original multiple regression model.
avPlots(model1) 

# Have a look at the extreme values
# Two are municipalities where ~40% of residents voted for CDA
# Two others have low percentage of highly educted residents and a relative high mean income
Data[c(76,300, 307, 255),]




#### Model 1a ####
# Check model1 when obs 300 and 76 are removed
# These observations seemed to have a high influence
model1a <- lm(CDA ~ Urban_index + High_edu_perc + Mean_income + Perc_60plus + 
                Non_west, data = Data[-c(76,300, 307, 255),])

summary(model1a)


# Check assumptions 
par(mfrow = c(2,2))
plot(model1a, which = 1) # still heterogenity
plot(model1a, which = 2) # still right skewed
plot(model1a, which = 3) # non-normal.
plot(model1a, which = 4) # lot of influencial points

avPlots(model1a)





#### Model 1b ####
model1b <- lm(log10(CDA) ~ Urban_index + High_edu_perc + Mean_income + Perc_60plus +
                Non_west, data = Data)
summary(model1b)

# Check assumptions
par(mfrow = c(2,2))
plot(model1b, which = 1) # still heteroscadacity, 
qqPlot(model1b) # normally distributed
plot(model1b, which = 3) # both the fitted and residuals are more spread out
plot(model1b, which = 4)  # only obs 16(amsterdam) is influential. 
# Cutoff val for Cooks: 4 / (369 - 5 - 1) = 0.011


avPlots(model1b)




#### Model 2 ####
# Remove mean income (not significant)
model2 <- lm(log10(CDA) ~ Urban_index + High_edu_perc + Non_west + Perc_60plus,
             data = Data)

summary(model2)
avPlots(model2)

# compare to first model
# The RSS has not changed significant 
anova(model1b, model2)

# Check assumptions 
par(mfrow = c(2,2))
plot(model2, which = 1) # obs 16 influential
qqPlot(model2)          # normally
plot(model2, which = 3)  
plot(model2, which = 4) # cutoff Cooksdist: 4/(369-3-1) = 0.011. obs 16 influential

# Have closer look at extreme values
Data[c(16, 300, 239),]

# Check leverage points again, now numerically 
# 24/369 municipalities have high leverage
levs <- lm.influence(model2)$hat 
cutoff.lev <- 2*4/369 
levs[levs>cutoff.lev]

# Check for outliers
rstudent(model2) [abs(rstudent(model2)) > 2]
outlierTest(model2)




#### Model3 ####
# Remove highly educated and insert Mean_income again
# Now, mean income has significant effect. But in practice this effect is very small
# The estimate is -0.006
model3 <- lm(log10(CDA) ~ Urban_index + Mean_income + Non_west + Perc_60plus,
             data = Data)
summary(model3)


##### Compare all models ####
# We cannot directly compare model2 and model3
anova(model1b, model2) # not significant improvement
anova(model1b, model3) # model3 is improvement in compare the model1b, weird



# Backward elimination with the AIC
# Lower AIC is better
step(model1b) # results is model2

#### Conclusion model2 ####

##################### End script ###############################################