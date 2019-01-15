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
library(MASS)
library(faraway)
library(knitr)

#### Set up ####
rm(list = ls()) # empty work space
Data_CDA <- read.csv("1_clean_data/Clean_data_CDA_2018-12-21.csv", 
                 header = T, stringsAsFactors = F)



Data_CDA$Non_west <- as.factor(Data_CDA$Non_west) # needs to be recognized as factor
#row.names(Data_CDA) <- Data_CDA$Muni # change rownames to the municipalities

data.frame(summary(glm_CDA_5))



#### Model 1 ####
# Start with all explanatory variables
# Use binomial, logit link
glm_CDA_1 <- glm(cbind(CDA_abs, Total_abs - CDA_abs) ~ Urban_index + 
                   High_educated_frac + Mean_income +Non_west + Frac_60plus, 
                 family=binomial(link = "logit"), data = Data_CDA)


summary(glm_CDA_1)



#### Model 2 ####
# Use quassi binomial, because we observe overdispersion
glm_CDA_2 <- glm(cbind(CDA_abs, Total_abs - CDA_abs) ~ Urban_index + 
                   High_educated_frac + Mean_income + Non_west + Frac_60plus, 
               family=quasibinomial(link = "logit"), data = Data_CDA)

sum_mdl2 <- summary(glm_CDA_2)

MASS::dropterm(glm_CDA_2)
Anova(glm_CDA_2, type = "II")

drop1(glm_CDA_2, scale = 254.0441, test = "F")

((sum_mdl2$null.deviance - sum_mdl2$deviance) / (sum_mdl2$df.null - sum_mdl2$df.residual)) / (sum_mdl2$dispersion)




#### glm_CDA_3 ####
# Remove Frac_60plus
glm_CDA_3 <- glm(cbind(CDA_abs, Total_abs - CDA_abs) ~ Urban_index + 
                   High_educated_frac + Mean_income + Non_west, 
              family=quasibinomial(link = "logit"), Data_CDA)

summary(glm_CDA_3)
Anova(glm_CDA_3, type = "II")







#### glm_CDA_4 ####
# Remove Mean_income
glm_CDA_4 <- glm(cbind(CDA_abs, Total_abs - CDA_abs) ~ Urban_index  + 
                   High_educated_frac + Non_west, 
                 family=quasibinomial(link = "logit"), data = Data_CDA)

summary(glm_CDA_4)
Anova(glm_CDA_4, type = "II")

vif(glm_CDA_4)
avPlots(glm_CDA_4)


# Assumptions
par(mfrow = c(2,2))
plot(glm_CDA_4, which = 1, id.n = 5) 
plot(glm_CDA_4, which = 3, id.n = 5) 
plot(glm_CDA_4, which = 4, id.n = 5)
abline(h = 0.011, col = "red")



#### glm_CDA_5 ####
# Remove obs 16 (amsterdam)
glm_CDA_5 <- glm(cbind(CDA_abs, Total_abs - CDA_abs) ~ Urban_index  + 
                   High_educated_frac + Non_west, 
                 family=quasibinomial(link = "logit"), 
                 data = Data_CDA[-c(16, 265),])

sum_mdl5 <- summary(glm_CDA_5)
Anova(glm_CDA_5, type = "II")
drop1(glm_CDA_5, scale = sum_mdl5$dispersion , test = "F")
avPlots(glm_CDA_5)

# Calculate F-test
((sum_mdl5$null.deviance - sum_mdl5$deviance) / (sum_mdl5$df.null - sum_mdl5$df.residual)) / (sum_mdl5$dispersion)

# Assumptions
par(mfrow = c(1,3))
plot(glm_CDA_5, which = 1, id.n = 5) 
plot(glm_CDA_5, which = 3, id.n = 5) 
halfnorm(residuals(glm_CDA_5,"pearson"))
plot(glm_CDA_5, which = 4, id.n = 5)
abline(h = 0.011, col = "red")


mean_hat <- mean(influence(glm_CDA_5)$hat)
cooks.distance(glm_CDA_5)
#################### End script ################################################