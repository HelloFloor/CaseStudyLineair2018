################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Cleaning_age.R
# Purpose of script:        Amount of 60+ residents per municipality
# Datafiles used:	          Clean_data_all_variables_2018-12-14.csv
#                           Leeftijd_groepen.csv
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-14
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)
library(tidyverse)


#### Set up ####
rm(list = ls()) # empty work space
cleandat <- read.csv("1_clean_data/Cleandata_all_variables_2018-12-06.csv", 
                 header = T, stringsAsFactors = F)

Data <- read.csv("0_data/Leeftijd_groepen.csv",
                 header = T, stringsAsFactors = F)

# We need one column with total amount of residents per municipality
# And one column with amount of 60 years or older residents per munipality
total <- Data[Data$Age == "Totaal",]

# Create wide format for Age groups
Data_wd <- Data %>% 
  spread(key = Age, value = Amount)

Data_wd <- Data_wd %>%
  rename(Total = Totaal) 
  
Data_wd$Old <- rowSums(Data_wd[,c(2:3)])
Data_wd$Perc_Old <- Data_wd$Old / Data_wd$Total
Data_wd <- Data_wd[,c(1,12)] # remove all columns that we do not need


# Merge datafiles
Data <- merge(cleandat, Data_wd, by = "Muni")


#### Save data ####
write.table(Data, file = paste("1_clean_data/Cleandata_all_variables_", 
                               Sys.Date(),".csv", sep = ""), sep = ",", 
            row.names = FALSE, na = "", col.names = T)
