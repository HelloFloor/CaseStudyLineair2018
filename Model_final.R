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
library(lindia)
library(MASS)
<<<<<<< HEAD
library(knitr)
=======
library(xtable)
>>>>>>> 0c13089f013a26a8535aa58977e8a451017e7c78

#### Set up ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/Cleandata_CDA_2018-12-14.csv", 
                 header = T, stringsAsFactors = F)


Data$Non_west <- as.factor(Data$Non_west) # needs to be recognized as factor
row.names(Data) <- Data$Muni # change rownames to the municipalities
Data$CDA_perc <- round(Data$CDA * 100, digits = 0)
Data$Voted_other <- 100 - Data$CDA_perc



#### Model 1 ####
# Start with all explanatory variables
model1 <- lm(CDA ~ Urban_index + High_edu_perc + Mean_income +
                Non_west + Perc_60plus,data = Data)
<<<<<<< HEAD
=======

>>>>>>> 0c13089f013a26a8535aa58977e8a451017e7c78

Anova(model1, type = "2")
step(model1)
summary(model1) 

avPlots(model1)

### Powerpoint
xtable(summary(model1))
png('Plots/avPlots1.png', width = 15, height = 7, units='in',res=600)
print(avPlots(model1))
dev.off()

# Check assumptions 
par(mfrow = c(1,2))
plot(model1, which = 1, id.n = 5) # we see heterogenitiy of variance
qqPlot(model1) # right tail is skewed (non normality)

plot(model1, which = 3, id.n = 5) # var of error not equal, non-linearity, obs 300 & 76 outliers
plot(model1, which = 4, id.n = 5)# cutt off val: 0.011. Everything above cut-off is outlier
abline(h = 0.011, col = "red")




# Residuals vs fitted values explanation:
#  1 The residuals "bounce randomly" around the 0 line. This suggests that the assumption
#    that the relationship is linear is reasonable.
#  2. The residuals roughly form a "horizontal band" around the 0 line. This suggests 
#     that the variances of the error terms are equal.
#  3. No one residual "stands out" from the basic random pattern of residuals. 
#   This suggests that there are no outliers.
#     

#### Diff influential point & outlier ####
# An outlier is a data point that diverges from an overall pattern in a sample. An outlier has a large residual (the distance between the predicted value () and the observed value (y)). Outliers lower the significance of the fit of a statistical model because they do not coincide with the model's prediction. An influential point is any point that has a large effect on the slope of a regression line fitting the data. They are generally extreme values. 



# CoxBox, because we see heterogenity of variance
# The Boxcox gives suggestions for transformation of the response
# The 95% Confidence interval is located around the 0. Meaning: log transformation
gg_boxcox(model1)



# Added variable plots 
# Used to investigate joint influence (see meeting 8)
# We already found out that our X variables are correlated. 
# In the model The estimates of the Xs suffer from omitted-variable bias
# Therefore, normal simple regressions are not sufficient (what we did in data visualisation)
# You can now look at the relationship between Y and X2 once all other predictors have been accounted for. So for example, the slope you can see in each plot now reflects the partial regression coefficients from your original multiple regression model.
# Bijv: de plot CDA (yas) en Mean income (x-as). Laat zien dat Tubbergen en
# Dinkelland outliers zijn. wassenaar en Bloemendaal hebben een hoge cooks distance (cooks kijkt naar effect of y & x-as), maar geen hoge leverage (leverage kijkt alleen naar effect op x-as)
# Mijn conclusie is dat de correlatie tussen de verschillende Xs en Y in het model wel meevalt. Dit is gunstig!
avPlots(model1) 

# Have a look at the extreme values
# Two are municipalities where ~40% of residents voted for CDA
# Two others have low percentage of highly educted residents and a relative high mean income
# Dit kan ook in het report
Data[c(76,300, 307, 255),]




#### Model 1a ####
# Check model1 when obs 300 and 76 are removed
# These observations are outliers
# Model1a niet in report! Is niet belangrijk voor model selectie stappen.
model1a <- lm(CDA ~ Urban_index + High_edu_perc + Mean_income + Perc_60plus + 
                Non_west, data = Data[-c(76,300, 307, 255),])

summary(model1a)

model1 <- lm(CDA ~ Urban_index + Perc_60plus + 
                Non_west, data = Data)

summary(model1)
anova(model1)
Anova(model1, type = "II")

# Check assumptions 
par(mfrow = c(1,2))
plot(model1a, which = 1) # still heterogenity
qqPlot(model1a) # still right skewed
plot(model1a, which = 3) # non-normal.
plot(model1a, which = 4) # lot of influencial points
abline(h = 0.011, col = "red")

# CHeck added variable plots
avPlots(model1a)





#### Model 1b ####
# BoxCox suggest that we need to log transform the respons
model1b <- lm(log10(CDA) ~ Urban_index + High_edu_perc + Mean_income + Perc_60plus +
                Non_west, data = Data)

summary(model1b)

# Check assumptions
par(mfrow = c(1,2))
plot(model1b, which = 1) # still heteroscadacity, 
qqPlot(model1b) # normally distributed
plot(model1b, which = 3) # both the fitted and residuals are more spread out
plot(model1b, which = 4)  # only obs 16(amsterdam) is influential. 
# Cutoff val for Cooks: 4 / (369 - 5 - 1) = 0.011

# Check added variable plots
# Seems like the residuals are more spread due to the log transformation
avPlots(model1b)



#### Model 1c ####
model1c <- glm(cbind(CDA_perc, Voted_other) ~ Urban_index + High_edu_perc + 
                 Mean_income +Non_west + Perc_60plus, 
               family=binomial,data = Data)

# Everything is significant and estimate of Perc_60plus is very large/unusual
summary(model1c)





#### Model 2 ####
# We continue with model1 (no transformation and normal lm model)
# Remove mean income (not significant)
model2 <- lm(CDA ~ Urban_index + High_edu_perc + Non_west + Perc_60plus,
             data = Data)

summary(model2)
# ANOVA table shows that we have a large RSS (within group variance)
Anova(model2, type = "II") # unbalanced dataset

xtable(Anova(model2, type = "II"))

# Check added variable plots
avPlots(model2)



# compare to first model
# The RSS has not changed significant 
anova(model1, model2)

# Check assumptions 
par(mfrow = c(1,2))
plot(model2, which = 1) # heteroscadacity, amsterdam oostzaan outliers
qqPlot(model2)          # normally
plot(model2, which = 3)  
plot(model2, which = 4) # cutoff Cooksdist: 4/(369-3-1) = 0.011. Amsterdam Utrecht, Urk influential
abline(h = 0.011, col = "red")

# Have closer look at extreme values
# Hierbij kan in het verslag worden uitgelegd, waarom bepaalde steden outliers/influential zijn
Data[c(16, 300, 239),]






#### Model3 ####
<<<<<<< HEAD
# Remove highly educated 
=======
# Remove highly educated and insert Mean_income again
# Now, mean income has significant effect. But in practice this effect is very small
# The estimate is -0.006
>>>>>>> 0c13089f013a26a8535aa58977e8a451017e7c78
model3 <- lm(CDA ~ Urban_index + Non_west + Perc_60plus,
             data = Data)

summary(model3)
<<<<<<< HEAD
Anova(model3, type = "II")

# Leverage = influential on the X-axis
# Check leverage points again, now numerically 
levs <- lm.influence(model3)$hat 
cutoff.lev <- 2*3/369 
high_lev <- levs[levs>cutoff.lev] # 35/369 municipalities have high leverage
=======
xtable(summary(model3))
#### Powerpoint
png('Plots/avPlots3.png', width = 15, height = 7, units='in',res=600)
print(avPlots(model3))
dev.off()
>>>>>>> 0c13089f013a26a8535aa58977e8a451017e7c78


# Check for outliers numerically
rstudent(model3) [abs(rstudent(model3)) > 2]
outlierTest(model3)



##### Compare all models ####
anova(model1, model2) # not significant improvement
anova(model2, model3) # model3 is improvement 


# Backward elimination with the AIC
# Lower AIC is better
# Important for report: we compared the models using anova and AIC
step(model1) # results is model1

#### Conclusion model3 ####

##################### End script ###############################################