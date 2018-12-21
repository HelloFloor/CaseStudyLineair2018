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


#### Set up ####
rm(list = ls()) # empty work space

Data_all <- read.csv("1_clean_data/Clean_data_all_variables_2018-12-21.csv",
                     stringsAsFactors=F, header = T)
Data_CDA <- read.csv("1_clean_data/Clean_data_CDA_2018-12-21.csv",
                     stringsAsFactors=F, header = T)
Data_GL <- read.csv("1_clean_data/Clean_data_GL_2018-12-21.csv",
                    stringsAsFactors=F, header = T)
row.names(Data) <- Data$Muni
#### Model 1 ####
# Start with all explanatory variables
lm_CDA_1 <- lm(CDA_frac ~ High_educated_frac +  Urban_index+
               Mean_income +Non_west + Frac_60plus,data = Data_CDA)

vif(lm_CDA_1)
step(lm_CDA_1)
summary(lm_CDA_1) 

avPlots(lm_CDA_1)


# Check assumptions 
par(mfrow = c(1,2))
plot(lm_CDA_1, which = 1, id.n = 5) # we see heterogenitiy of variance
qqPlot(lm_CDA_1) # right tail is skewed (non normality)

plot(lm_CDA_1, which = 3, id.n = 5) # var of error not equal, non-linearity, obs 300 & 76 outliers
plot(lm_CDA_1, which = 4, id.n = 5)# cutt off val: 0.011. Everything above cut-off is outlier
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
gg_boxcox(lm_CDA_1)



# Added variable plots 
# Used to investigate joint influence (see meeting 8)
# We already found out that our X variables are correlated. 
# In the model The estimates of the Xs suffer from omitted-variable bias
# Therefore, normal simple regressions are not sufficient (what we did in data visualisation)
# You can now look at the relationship between Y and X2 once all other predictors have been accounted for. So for example, the slope you can see in each plot now reflects the partial regression coefficients from your original multiple regression model.
# Bijv: de plot CDA (yas) en Mean income (x-as). Laat zien dat Tubbergen en
# Dinkelland outliers zijn. wassenaar en Bloemendaal hebben een hoge cooks distance (cooks kijkt naar effect of y & x-as), maar geen hoge leverage (leverage kijkt alleen naar effect op x-as)
# Mijn conclusie is dat de correlatie tussen de verschillende Xs en Y in het model wel meevalt. Dit is gunstig!


# Have a look at the extreme values
# Two are municipalities where ~40% of residents voted for CDA
# Two others have low percentage of highly educted residents and a relative high mean income
# Dit kan ook in het report
Data_CDA[c(76,300, 307, 255),]




#### Model 1a ####
# Check model1 when obs 300 and 76 are removed
# These observations are outliers
# Model1a niet in report! Is niet belangrijk voor model selectie stappen.
lm_CDA_1a <- lm(CDA_frac ~ High_educated_frac +  Urban_index+
                Mean_income +Non_west + Frac_60plus,data = Data_CDA[-c(76,300, 307, 255),])
  

summary(lm_CDA_1a)


# Check assumptions 
par(mfrow = c(1,2))
plot(lm_CDA_1a, which = 1) # still heterogenity
qqPlot(lm_CDA_1a) # still right skewed
plot(lm_CDA_1a, which = 3) # non-normal.
plot(lm_CDA_1a, which = 4) # lot of influencial points
abline(h = 0.011, col = "red")

# CHeck added variable plots
avPlots(model1a)





#### Model 1b ####
# BoxCox suggest that we need to log transform the respons
lm_CDA_1b <- lm(log10(CDA_frac) ~ High_educated_frac +  Urban_index+
                  Mean_income +Non_west + Frac_60plus,data = Data_CDA)

summary(lm_CDA_1b)

# Check assumptions
par(mfrow = c(1,2))
plot(lm_CDA_1b, which = 1) # still heteroscadacity, 
qqPlot(lm_CDA_1b) # normally distributed
plot(lm_CDA_1b, which = 3) # both the fitted and residuals are more spread out
plot(lm_CDA_1b, which = 4)  # only obs 16(amsterdam) is influential. 
# Cutoff val for Cooks: 4 / (369 - 5 - 1) = 0.011

# Check added variable plots
# Seems like the residuals are more spread due to the log transformation
avPlots(model1b)

lm_CDA_1c<- lm(log10(CDA_frac) ~ High_educated_frac +  Urban_index+
                 Mean_income +Non_west + Frac_60plus,data = Data_CDA[-16,])

summary(lm_CDA_1c)


#### Model 2 ####
# Remove mean income (not significant)
lm_CDA_2 <-lm(log10(CDA_frac) ~ High_educated_frac +  Urban_index+
                Non_west + Frac_60plus,data = Data_CDA)

summary(lm_CDA_2)
# ANOVA table shows that we have a large RSS (within group variance)
Anova(lm_CDA_2, type = "II") # unbalanced dataset


# Check added variable plots
avPlots(lm_CDA_2)



# compare to first model
# The RSS has not changed significant 
anova(lm_CDA_1b, lm_CDA_2)

# Check assumptions 
par(mfrow = c(1,2))
plot(lm_CDA_2, which = 1) # heteroscadacity, amsterdam oostzaan outliers
qqPlot(lm_CDA_2)          # normally
plot(lm_CDA_2, which = 3)  
plot(lm_CDA_2, which = 4) # cutoff Cooksdist: 4/(369-3-1) = 0.011. Amsterdam Utrecht, Urk influential
abline(h = 0.011, col = "red")

# Have closer look at extreme values
# Hierbij kan in het verslag worden uitgelegd, waarom bepaalde steden outliers/influential zijn
Data[c(16, 300, 239),]

# Leverage = influential on the X-axis
# Check leverage points again, now numerically 
levs <- lm.influence(model2)$hat 
cutoff.lev <- 2*5/369 
high_lev <- levs[levs>cutoff.lev] # 35/369 municipalities have high leverage

# Check for outliers numerically
rstudent(model2) [abs(rstudent(model2)) > 2]
outlierTest(lm_CDA_2)

### Remove outlier 16, 237, 239 , RSS wordt niet veel kleiner, dus daarom bij lm_CDA_2 gebleven
lm_CDA_2b <-lm(log10(CDA_frac) ~ High_educated_frac +  Urban_index+
                Non_west + Frac_60plus,data = Data_CDA[-16,])

summary(lm_CDA_2b)


anova(lm_CDA_2, lm_CDA_2b)

outlierTest(lm_CDA_2b)

#### Model3 ####
# Remove highly educated and insert Mean_income again
# Now, mean income has significant effect. But in practice this effect is very small
# The estimate is -0.006
lm_CDA_3 <- lm(log10(CDA_frac) ~  Urban_index+ Mean_income+
                  Non_west + Frac_60plus,data = Data_CDA)
summary(lm_CDA_3)

par(mfrow = c(1,2))
plot(lm_CDA_3, which = 1) # heteroscadacity, amsterdam oostzaan outliers
qqPlot(lm_CDA_3)          # normally
plot(lm_CDA_3, which = 3)  
plot(lm_CDA_3, which = 4) # cutoff Cooksdist: 4/(369-3-1) = 0.011. Amsterdam Utrecht, Urk influential
abline(h = 0.011, col = "red")


### Remove outlier 16
lm_CDA_3b <- lm(log10(CDA_frac) ~  Urban_index+ Mean_income+
                 Non_west + Frac_60plus,data = Data_CDA[-16,])
summary(lm_CDA_3b)

par(mfrow = c(1,2))
plot(lm_CDA_3b, which = 1) # heteroscadacity, amsterdam oostzaan outliers
qqPlot(lm_CDA_3b)          # normally
plot(lm_CDA_3b, which = 3)  
plot(lm_CDA_3b, which = 4) # cutoff Cooksdist: 4/(369-3-1) = 0.011. Amsterdam Utrecht, Urk influential
abline(h = 0.011, col = "red")

avPlots(lm_CDA_3b)
vif(lm_CDA_3b)


levs <- lm.influence(lm_CDA_3b)$hat 
cutoff.lev <- 2*4/369 
levs[levs>cutoff.lev]
##### Compare all models ####
# We cannot directly compare model2 and model3
anova(lm_CDA_1b, lm_CDA_2) # not significant improvement
anova(lm_CDA_1b, lm_CDA_3) # model3 is improvement in compare the model1b, weird


anova(lm_CDA_1c, lm_CDA_2b) # not significant improvement
anova(lm_CDA_1c, lm_CDA_3b) # model3 is improvement in compare the model1b, weird


# Backward elimination with the AIC
# Lower AIC is better
# Important for report: we compared the models using anova and AIC
step(lm_CDA_1c) # results is model2

#### Conclusion model3 ### 


##################### End script ###############################################