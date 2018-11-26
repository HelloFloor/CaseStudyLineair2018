
################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Floor Komen 
# Script:                   First_models.R
# Purpose of script:        Histogrammen en eerste lineair models 
#
# Datafiles used:	          voting_and_demographics.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-11-25
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(plyr)
library(dplyr)
library(compare)
library(foreign)
library(data.table)
library(emmeans)

#### Import data #### 
CleanData <- read.csv("1_clean_data/voting_and_demographics.csv",stringsAsFactors=F)

#### Data-inspectie met behulp van histogrammen en qqplots
politieke_partijen <- c("VVD","CDA","PVV", "D66", "SP", "GL","PvdA","CU","50+","PvdD","SGP","FvD","DENK")
politieke_kleur <- c("darkblue","darkgreen","lightblue","lightgreen","tomato","forestgreen","orangered","yellow","purple","pink","gray50","brown","turquoise")
names(politieke_kleur) <- politieke_partijen
### Plot stedelijkheid
plot(c(0,4),c(0,0.5),type='n',xlab="Stedelijkheid",ylab = "Aandeel stemmen")
for (partij in politieke_partijen) {
  abline(lm(CleanData[,partij]~CleanData$stedelijkheidsindex.x)$coefficients, col=politieke_kleur[partij])
} 
legend("topleft",politieke_partijen,col = politieke_kleur,lty = 1)

### Plot Percentage Highly Educated 
plot(c(0,1),c(0,0.5),type='n',xlab="Percentage hoogopgeleide",ylab = "Aandeel stemmen")
for (partij in politieke_partijen) {
  abline(lm(CleanData[,partij]~CleanData$Percentage.highly.educated)$coefficients, col=politieke_kleur[partij])
} 
legend("topleft",politieke_partijen,col = politieke_kleur, lty=1)

### Plot Average Income 
plot(c(20,42.6),c(0,0.5),type='n',xlab="Gemiddeld inkomen",ylab = "Aandeel stemmen")
for (partij in politieke_partijen) {
  abline(lm(CleanData[,partij]~CleanData$Average.income.in..1000)$coefficients, col=politieke_kleur[partij])
} 
legend("topleft",politieke_partijen,col = politieke_kleur, lty=1)

### Plot Nederlandse etniciteit 
plot(c(0,1),c(0,0.5),type='n',xlab="Percentage Nederlandse Herkomst",ylab = "Aandeel stemmen")
for (partij in politieke_partijen) {
  abline(lm(CleanData[,partij]~CleanData$Dutch_perc)$coefficients, col=politieke_kleur[partij])
} 
legend("topleft",politieke_partijen,col = politieke_kleur, lty=1)

### Plot Niet westerse etniciteit 
plot(c(0,1),c(0,0.5),type='n',xlab="Percentage Niet Westerse Herkomst",ylab = "Aandeel stemmen")
for (partij in politieke_partijen) {
  abline(lm(CleanData[,partij]~CleanData$Non_west_perc)$coefficients, col=politieke_kleur[partij])
} 
legend("topleft",politieke_partijen,col = politieke_kleur, lty=1)

### Plot westerse buitenlandse etniciteit
plot(c(0,1),c(0,0.5),type='n',xlab="Percentage Westerse Buitenlandse Herkomst",ylab = "Aandeel stemmen")
for (partij in politieke_partijen) {
  abline(lm(CleanData[,partij]~CleanData$West_perc)$coefficients, col=politieke_kleur[partij])
} 
legend("topleft",politieke_partijen,col = politieke_kleur, lty=1)


#### First regression model 
#### Lineair model: Percentage_partij= B0 + B1 * Stedelijheidsindex + B2* Percentage Highly Educated + 
#### B3 * Average income + B4 * Dutch percentage + B5* non_west_perc
## Deze onderstaande manieren werken dus niet, werken wel bij de grafieken ook al krijg ik dan een foutmelding
## Maar bij de grafieken print die wel iets, bij onderstaande print die niks door de error.
for (partij in  politieke_partijen){
  coef(summary(lm(partij ~ Percentage.highly.educated , data=CleanData)))
}


for (partij in  politieke_partijen){
  coef(summary(lm(CleanData[,partij] ~ CleanData$stedelijkheidsindex.x + CleanData$Percentage.highly.educated + CleanData$Average.income.in..1000 + CleanData$Dutch_perc + CleanData$Non_west_perc)))
}  

#### VVD: Stedelijkheid, Nederlandse & niet westerse etniciteit niet significant. 
lm_VVD <- lm(VVD ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_VVD))
anova(lm_VVD)

#### CDA : Stedelijkheid, etniciteit niet significant, andere wel bij t-toets.
### Bij ANOVA average income en non_west niet significant
lm_CDA <- lm(CDA ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_CDA))
anova(lm_CDA)

#### PVV: Stedelijkheid en average income niet significant bij t-toets
### Bij ANOVA alleen stedelijkheid niet significant 
lm_PVV <- lm(PVV ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_PVV))
anova(lm_PVV)

### D66: Stedelijkheid, average income en non_west_perc niet significant bij t-toets
### Average income en non_west_perc niet significant bij ANOVA 
lm_D66 <- lm(D66 ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_D66))
anova(lm_D66)

#### SP: Educaie niet significant t-toets
### Alles significant bij Anova
lm_SP <- lm(SP ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_SP))
anova(lm_SP)

#### GL: Stedelijkehid en dutch niet significant t-toets
### Alles significant ANOVA 
lm_GL <- lm(GL ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_GL))
anova(lm_GL)

#### PVDA : t-toets alles significant
### Anova Stedelijkheid en Dutch_perc niet significant 
lm_PvdA <- lm(PvdA ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_PvdA))
anova(lm_PvdA)

####CU :Educatie en inkomen niet significant t-toets
### Inkomen niet significant ANOVA 
lm_CU <- lm(CU ~ stedelijkheidsindex.x + Percentage.highly.educated + Average.income.in..1000 + Dutch_perc + Non_west_perc, data=CleanData)
coef(summary(lm_CU))
anova(lm_CU)