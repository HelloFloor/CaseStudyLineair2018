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

Data_all <- read.csv("1_clean_data/Clean_data_all_variables_2018-12-21.csv",
                     stringsAsFactors=F, header = T)
Data_CDA <- read.csv("1_clean_data/Clean_data_CDA_2018-12-21.csv",
                     stringsAsFactors=F, header = T)
Data_GL <- read.csv("1_clean_data/Clean_data_GL_2018-12-21.csv",
                    stringsAsFactors=F, header = T)
#row.names(Data) <- Data$Muni
summary(Data)

#### Model1 ####
lm_GL_1 <- lm(GL_frac ~ High_educated_frac +  Urban_index+
                 Mean_income +Non_west + Frac_60plus,data = Data_GL)


summary(lm_GL_1)
vif(lm_GL_1)
# Check assumptions 
par(mfrow = c(2,2))
plot(lm_GL_1, which = 1)
plot(lm_GL_1, which = 2)
plot(lm_GL_1, which = 3)
plot(lm_GL_1, which = 4) 
abline(h = 0.011, col = "red")



avPlots(lm_GL_1)
gg_boxcox(lm_GL_1)

# GL needs a sqrt transformation

#### Model1a ####
# Transformation of response (sqrt)
lm_GL_1a <- lm(sqrt(GL_frac) ~ High_educated_frac +  Urban_index+
                Mean_income +Non_west + Frac_60plus,data = Data_GL)


summary(lm_GL_1a)

# Check assumptions 
par(mfrow = c(2,2))
plot(lm_GL_1a, which = 1)
qqPlot(lm_GL_1a)
plot(lm_GL_1a, which = 3)
plot(lm_GL_1a, which = 4)
abline(h = 0.011, col = "red")

avPlots(lm_GL_1a)
# Have closer look at extreme values
Data[c(42, 307,308),]





#### Model2 ####
# We remove Urbanity index 
lm_GL_2 <- lm(sqrt(GL_frac) ~ High_educated_frac +
                 Mean_income +Non_west + Frac_60plus,data = Data_GL)


summary(lm_GL_2)

# Check assumptions 
par(mfrow = c(2,2))
plot(lm_GL_2, which = 1)
qqPlot(lm_GL_2)
plot(lm_GL_2, which = 3)
plot(lm_GL_2, which = 4)
abline(h = 0.011, col = "red")

avPlots(lm_GL_2)

anova(lm_GL_1a, lm_GL_2) # Niet significant, hoewel urban index 0.7 is. 
step(lm_GL_1a) # Zegt dat Urban index erit moet 

# Check leverage points again, now numerically 
# 24/369 municipalities have high leverage
levs <- lm.influence(lm_GL_2)$hat 
cutoff.lev <- 2*4/369 
levs[levs>cutoff.lev]

# Check for outliers
rstudent(lm_GL_2) [abs(rstudent(lm_GL_2)) > 2]
outlierTest(lm_GL_2)





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