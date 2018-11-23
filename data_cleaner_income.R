################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Lotte Pater
# Project:                  Case Study Linear Models 
# Script:                   data_cleaner_income
# Purpose of script:        Convert CBS income data to something usable for our Case Study
# Datafiles used:	          gem_gestandaardiseerd_inkomen.csv
#                          
# Date:                     2018-11-23
# Version:  		    V.1.0					
################################################################################


####Read the data####
data <- read.csv("0_data/gem_gestandaardiseerd_inkomen.csv",sep=";")


####Make the data nicer####
data <- data[-389,] #removes a line consisting of text instead of data
data <- data[,-2] #removes the time variable, which is the same everywhere
colnames(data) <- c("Gemeente","Average income in ???1000")
data$Gemeente <- as.character(data$Gemeente)

####Save the data####
write.csv(data,"1_clean_data/income.csv")
