
################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Floor Komen 
# Script:                   First_models.R
# Purpose of script:        Histogrammen en eerste lineair modellen
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
colnames(CleanData) <- c("Gemeente", "VVD","CDA","PVV", "D66", "SP", "GL","PvdA","CU","50PLUS","PvdD","SGP","FvD","DENK", "stedelijkheidsindex.x", "stedelijkheidsindex.y", "Percentage.highly.educated", "Average.income.in..1000", "Dutch_perc", "West_perc", "Non_west_perc")

#### Data-inspectie met behulp van histogrammen, qqplots, Scatterplotmatrixen en lineaire regressie plot
politieke_partijen <- c("VVD","CDA","PVV", "D66", "SP", "GL","PvdA","CU","50PLUS","PvdD","SGP","FvD","DENK")
politieke_kleur <- c("darkblue","darkgreen","lightblue","lightgreen","tomato","forestgreen","orangered","yellow","purple","pink","gray50","brown","turquoise")
names(politieke_kleur) <- politieke_partijen
variabelen <- c("stedelijkheidsindex.x", "Percentage.highly.educated","Average.income.in..1000", "Dutch_perc","Non_west_perc","West_perc")

### Plot functie
plotfunction <- function(x1, x2, x3, politieke_partijen, politieke_kleur, data){
  plot(x1,x2,type='n',xlab=x3,ylab = "Aandeel stemmen")
  for (partij in politieke_partijen) {
    abline(lm(CleanData[,partij]~CleanData[[x3]])$coefficients, col=politieke_kleur[partij])
  } 
  legend("topleft",politieke_partijen,col = politieke_kleur,lty = 1)
}
###Plot Stedelijkheid
plotfunction(c(0,4), c(0, 0.5), "stedelijkheidsindex.x", politieke_partijen, politieke_kleur, CleanData)
###Plot opleiding
plotfunction(c(0,1), c(0,0.5),"Percentage.highly.educated", politieke_partijen, politieke_kleur, CleanData)
### Plot inkomen
plotfunction(c(20,42.6), c(0, 0.5), "Average.income.in..1000", politieke_partijen, politieke_kleur, CleanData)
###Plot nederlandse achtergrond
plotfunction(c(0,1), c(0, 0.5), "Dutch_perc", politieke_partijen, politieke_kleur, CleanData)
###Plot niet-westerse achtergrond
plotfunction(c(0,1), c(0, 0.5), "Non_west_perc", politieke_partijen, politieke_kleur, CleanData)
### Plot westerse achtergrond
plotfunction(c(0,1), c(0, 0.5), "West_perc", politieke_partijen, politieke_kleur, CleanData)


#Verschillende plots om te proberen. 
scatterplotMatrix(CleanData[,19:21])
for (variabele in variabelen) {
  qqPlot(CleanData[,variabele], ylab=variabele)
  boxplot(CleanData[, variabele], ylab=variabele)
}
hist(CleanData$Dutch_perc, na.rm=TRUE)
plot(density(CleanData$Dutch_perc,na.rm=TRUE))
pairs(CleanData[,19:21])
#### First regression model 
#### Lineair model: Percentage_partij= B0 + B1 * Stedelijheidsindex + B2* Percentage Highly Educated + 
#### B3 * Average income + B4 * Dutch percentage + B5* non_west_perc

lm_simple <- function(partij){
  lm(CleanData[,partij] ~ CleanData$stedelijkheidsindex.x + CleanData$Percentage.highly.educated + CleanData$Average.income.in..1000 + CleanData$Dutch_perc + CleanData$Non_west_perc, na.action=na.omit)
}

#### VVD: Stedelijkheid, Nederlandse & niet westerse etniciteit niet significant. 
lm_VVD <- lm_simple('VVD')
coef(summary(lm_VVD))
anova(lm_VVD)

#### CDA : Stedelijkheid, etniciteit niet significant, andere wel bij t-toets.
### Bij ANOVA average income en non_west niet significant
lm_CDA <- lm_simple("CDA")
coef(summary(lm_CDA))
anova(lm_CDA)

#### PVV: Stedelijkheid en average income niet significant bij t-toets
### Bij ANOVA alleen stedelijkheid niet significant 
lm_PVV <- lm_simple("PVV")
coef(summary(lm_PVV))
anova(lm_PVV)

### D66: Stedelijkheid, average income en non_west_perc niet significant bij t-toets
### Average income en non_west_perc niet significant bij ANOVA 
lm_D66 <- lm_simple("D66")
coef(summary(lm_D66))
anova(lm_D66)

#### SP: Educaie niet significant t-toets
### Alles significant bij Anova
lm_SP <- lm_simple("SP")
coef(summary(lm_SP))
anova(lm_SP)

#### GL: Stedelijkehid en dutch niet significant t-toets
### Alles significant ANOVA 
lm_GL <- lm_simple("GL")
coef(summary(lm_GL))
anova(lm_GL)

#### PVDA : t-toets alles significant
### Anova Stedelijkheid en Dutch_perc niet significant 
lm_PvdA <-lm_simple('PvdA')
coef(summary(lm_PvdA))
anova(lm_PvdA)
####CU :Educatie en inkomen niet significant t-toets
### Inkomen niet significant ANOVA 
lm_CU <- lm_simple("CU")
coef(summary(lm_CU))
anova(lm_CU)
####50PLUS
lm_50PLUS <- lm_simple("50PLUS")
coef(summary(lm_50PLUS))
anova(lm_50PLUS)
#### PvdD
lm_PvdD <- lm_simple("PvdD")
coef(summary(lm_PvdD))
anova(lm_PvdD)
#### SGP
lm_SGP <- lm_simple("SGP")
coef(summary(lm_SGP))
anova(lm_SGP)
#### FvD
lm_FvD <- lm_simple("FvD")
coef(summary(lm_FvD))
anova(lm_FvD)
#### DENK
lm_DENK <- lm_simple("DENK")
coef(summary(lm_DENK))
anova(lm_DENK)

################
###OVERIG NIET BELANGRIJK VOOR NU
#####
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
