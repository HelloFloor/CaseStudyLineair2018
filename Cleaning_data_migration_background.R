################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen, 
# Script:                   Cleaning data: migrant background
# Purpose of script:	      Computing right format to analyse the migrant 
#                           background data
#
# Datafiles used:	          Population_migrant_background.csv
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-11-23
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(plyr)
library(dplyr)
library(compare)
library(foreign)
library(data.table)


#### Input ####
rm(list = ls(all.names = TRUE))
options(warn = 1) 

data = read.table(file =  'Population_migrant_background.csv', 
                     sep = ',', header = T)

str(data)

# Remove the different age groups
# Make sure R recognized variables correctly
data <- data[data$Age == "Totaal", ]
data <- data[,2:4] # remove first column
data$Gemeente <- as.character(data$Gemeente)
data$Migrant_background <- as.character(data$Migrant_background)


#### Create new dataframes ####
# Each background get its own dataframe
total <- data[data$Migrant_background == "Totaal",]
total <- rename(total, Total = Amount )
total <- total[,c(1,3)]

dutch <- data[data$Migrant_background == "Nederlandse achtergrond",]
dutch <- rename(dutch, Dutch = Amount )
dutch <- dutch[,c(1,3)]

western <- data[data$Migrant_background == "Westerse migratieachtergrond",]
western <- rename(western, Western = Amount )
western <- western[,c(1,3)]

non_west <- data[data$Migrant_background == "Niet-westerse migratieachtergrond",]
non_west <- rename(non_west, Non_western = Amount )
non_west <- non_west[,c(1,3)]


# combine all dataframes
data_clean <- merge(total, dutch, by = "Gemeente")
data_clean <- merge(data_clean, western, by = "Gemeente")
data_clean <- merge(data_clean, non_west, by = "Gemeente")

# Compute percentages
data_clean$Dutch_perc <- round(data_clean$Dutch / data_clean$Total, digits = 2)
data_clean$West_perc <- round(data_clean$Western / data_clean$Total, digits = 2)
data_clean$Non_west_perc <- round(data_clean$Non_western / data_clean$Total, digits = 2)

# Remove column absolute amounts
data_clean <- data_clean[,-c(2:5)]

write.csv(data_clean,"1_clean_data/migrant_background.csv")
