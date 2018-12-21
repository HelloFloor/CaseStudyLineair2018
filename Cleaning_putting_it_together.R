################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Lotte Pater 
# Script:                   putting_it_together.R
# Purpose of script:        Merging voting & demographic data together
#
# Datafiles used:	          stemmen_percentages.csv; education.csv; income.csv; stedelijkheidsindex.csv; migrant_background.csv
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-11-23
# Version:  		            V.1.0					
################################################################################
rm(list = ls())

#### Libraries ####
library(plyr)
library(dplyr)
library(compare)
library(foreign)
library(data.table)

####Import data####
votes <- read.csv("1_clean_data/stemmen_percentages.csv",stringsAsFactors=F)
education <- read.csv("1_clean_data/education.csv",stringsAsFactors=F)
income <- read.csv("1_clean_data/income.csv",stringsAsFactors=F)
urban <- read.csv("1_clean_data/stedelijkheidsindex.csv",stringsAsFactors=F)
migrant_background <- read.csv("1_clean_data/migrant_background.csv",stringsAsFactors=F)


####Create a Data Frame Make the "Votes" data frame to a nice one####

data <- votes
colnames(data)[1] <- "Gemeente"
data <- votes[,-2] #removes total votes

####Merge the urbanity data in the data frame####
colnames(urban)[1] <- "Gemeente"
data <- merge(data,urban[,c(1,7)],by="Gemeente")

####Prepare the other variables#####

education <- education[education$Gemeente!="Boxmeer",] ##Boxmeer doesn't have voting data
income <- income[income$Gemeente!="Boxmeer",]
migrant_background <- migrant_background[migrant_background$Gemeente!="Boxmeer",]

migrant_background <- migrant_background[migrant_background$Gemeente!="Niet-gemeentelijk ingedeeld",] 
migrant_background[c(385,386,387),] <- NA ##Some data is missing, so we have to add empty rows to get
migrant_background$Gemeente <- c(migrant_background$Gemeente[1:384],"'s-Gravenhage (gemeente)","'s-Hertogenbosch","Nuenen, Gerwen en Nederwetten")

  
####Change the names for Gemeente in the voting data ####
data$Gemeente[data$Gemeente=="Utrecht"] <- "Utrecht (gemeente)"
data$Gemeente[data$Gemeente=="Groningen"] <- "Groningen (gemeente)"
data$Gemeente[data$Gemeente=="'s-Gravenhage"] <- "'s-Gravenhage (gemeente)"
data$Gemeente[data$Gemeente=="Middelburg"] <- "Middelburg (Z.)"
data$Gemeente[data$Gemeente=="Beek"] <- "Beek (L.)"
data$Gemeente[data$Gemeente=="Bergen (L)"] <- "Bergen (L.)"
data$Gemeente[data$Gemeente=="Bergen (NH)"] <- "Bergen (NH.)"
data$Gemeente[data$Gemeente=="Hengelo"] <- "Hengelo (O.)"
data$Gemeente[data$Gemeente=="Laren"] <- "Laren (NH.)"
data$Gemeente[data$Gemeente=="Rijswijk"] <- "Rijswijk (ZH.)"
data$Gemeente[data$Gemeente=="Stein"] <- "Stein (L.)"

####Merge the other variables in####
data <- merge(data,education[,c(2,3)],by="Gemeente")
data$Percentage.highly.educated <- data$Percentage.highly.educated*0.01
data <- merge(data,income[,c(2,3)],by="Gemeente")
data <- merge(data,migrant_background[,-1],by="Gemeente")


####Save the data as one file####
write.csv(data,"1_clean_data/voting_and_demographics.csv",row.names=FALSE)
