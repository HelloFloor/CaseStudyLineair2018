################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Lotte Pater
# Project:                  Case Study Linear Models 
# Script:                   data_cleaner_education
# Purpose of script:        Convert CBS data concerning the percentage of persons with higher education to something usable for our Case Study
# Datafiles used:	          hoogopgeleiden2017.csv
#                          
# Date:                     2018-11-23
# Version:  		    V.1.1					
################################################################################

#####Read the data####
data <- read.csv("0_data/hoogopgeleiden2017.csv",sep=";")

####Make the data nicer####
data <- data[-389,1:3] #removes empty rows and columns
data <- data[,-2] #removes an irrelevant row
colnames(data) <- c("Gemeente","Percentage highly educated")
data$Gemeente <- as.character(data$Gemeente)
data$`Percentage highly educated` <- as.numeric(as.character(data$`Percentage highly educated`)) ##Note that converting to characters first is neccesary, otherwise R will find the level numbers


####Save the data####
write.csv(data,"1_clean_data/education.csv")
