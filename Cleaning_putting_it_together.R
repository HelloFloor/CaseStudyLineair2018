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
education <- read.csv("1_clean_data/education.csv",
                      stringsAsFactors=F)
income <- read.csv("1_clean_data/income.csv",
                   stringsAsFactors=F)
urban <- read.csv("1_clean_data/stedelijkheidsindex.csv",
                  stringsAsFactors=F)
migrant_background <- read.csv("1_clean_data/migrant_background.csv",
                               stringsAsFactors=F)
votes_raw <- read.csv("0_data/stemmen_aantallen.csv", 
                      stringsAsFactors = F, header = T)

age <- read.csv("0_data/Leeftijd_groepen.csv", 
                stringsAsFactors = F, header = T)
####Create a Data Frame Make the "Votes" data frame to a nice one####

data <- votes
colnames(data)[1] <- "Gemeente"
data <- votes[,-2] #removes total votes

####Merge the urbanity data in the data frame####
colnames(urban)[1] <- "Gemeente"
colnames(data)[1] <- "Gemeente"
data <- merge(data, urban[,c(1,7)],by="Gemeente")

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
data <- merge(data,migrant_background[,c(1,4)],by="Gemeente")


#### Remove parties that we do not use ####
data <- data[ ,-c(2,4:6,8:14)]


# Add absolute voting results 
votes_raw$Votes_total <- rowSums(votes_raw[2:14])
votes_raw <- votes_raw[-c(2,4:6,8:14)]
colnames(votes_raw) <- c("Gemeente", "CDA_abs", "GL_abs", "Total_abs")

# Merge raw data and data
data2 <- merge(data, votes_raw, by = "Gemeente") 
colnames(data2) <- c("Muni", "CDA_frac" ,"GL_frac", "Urban_index" , "High_educated_frac", "Mean_income", "Non_west_frac" , "CDA_abs"  ,"GL_abs",  "Total_abs" )

data2[, c(2:4)] <- round(data2[2:4], digits = 3)



# Non_west_perc is not linear. Therefore, we create a dummy variable with 3 levels
# Level 1: x < 5%
# Level 2: 5 <= x < 10%
# Level 3: x >= 10%
# Explain this in report
Data <- data2
Data$Non_west <- ifelse(Data$Non_west_frac < 0.05, 1, NA)
Data$Non_west <- ifelse(Data$Non_west_frac >= 0.05 
                        & Data$Non_west_frac < 0.1, 2, Data$Non_west)
Data$Non_west <- ifelse(Data$Non_west_frac >= 0.1, 3, Data$Non_west)

# change to factor and remove NAs
Data$Non_west <- as.factor(Data$Non_west)
Data <- na.omit(Data)
Dat_all <- Data

# Add 60+ residents and merge again
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
Data_wd$Frac_60plus <- round(Data_wd$Old / Data_wd$Total, digits = 2)
Data_wd <- Data_wd[,c(1,12)] # remove all columns that we do not need


# Merge datafiles
Data <- merge(Dat_all, Data_wd, by = "Muni")



###Save the data as one file####
# all variables
write.csv(Data,paste0("1_clean_data/Clean_data_all_variables_", 
                       Sys.Date(), ".csv"), row.names=FALSE)


# CDA variables
write.csv(Data[,-c(3,9)],paste0("1_clean_data/Clean_data_CDA_", 
                       Sys.Date(), ".csv"), row.names=FALSE)

# GL variables
write.csv(Data[,-c(2,8)],paste0("1_clean_data/Clean_data_GL_", 
                       Sys.Date(), ".csv"), row.names=FALSE)

###################### End script ##############################################