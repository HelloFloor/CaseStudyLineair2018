################################################################################
# Run-Time Environment:     R version 3.4.2
# Author:                   Ilse van Beelen 
# Script:                   Data_visualisation.R
# Purpose of script:        Data cleaning, renaming & visualisation
#
# Datafiles used:	          voting_and_demographics.csv;
# Data downloaded:          Data downloaded from statline.cbs.nl
#                          
# Date:                     2018-12-05
# Version:  		            V.1.0					
################################################################################

#### Libraries ####
library(car)
library(ggplot2)
library(reshape2)
library(plyr)
library(GGally)
library(Hmisc)
library(lattice)
library(survival)

#### Import data ####
rm(list = ls()) # empty work space
Data <- read.csv("1_clean_data/voting_and_demographics.csv",
                 stringsAsFactors=F, header = T)

# Rename and remove columns
Data <- Data[,-15]
colnames(Data) <- c("Muni", "VVD","CDA","PVV", "D66", "SP", "GL","PvdA","CU","50PLUS","PvdD","SGP","FvD","DENK", "Urban_index", "High_edu_perc", "Mean_income", "Dutch_perc", "West_perc", "Non_west_perc")

# Non_west_perc is not linear. Therefore, we create a dummy variable with 3 levels
# Level 1: x < 5%
# Level 2: 5 <= x < 10%
# Level 3: x >= 10%
Data$Non_west <- ifelse(Data$Non_west_perc < 0.05, 1, NA)
Data$Non_west <- ifelse(Data$Non_west_perc >= 0.05 
                           & Data$Non_west_perc < 0.1, 2, Data$Non_west)
Data$Non_west <- ifelse(Data$Non_west_perc >= 0.1, 3, Data$Non_west)

Data$Non_west <- as.factor(Data$Non_west)

# Select the right variables and remove NAs
Dat_cda <- Data[,c(1,3,15,16,17,21)]
Dat_cda <- Dat_cda[complete.cases(Dat_cda),]



#### Save final dataframes ####
write.table(Data, file = paste("1_clean_data/Cleandata_all_variables_", 
                               Sys.Date(),".csv", sep = ""), sep = ",", 
            row.names = FALSE, na = "", col.names = T)

write.table(Dat_cda, file = paste("1_clean_data/Cleandata_CDA_", 
                                  Sys.Date(),".csv", sep = ""), sep = ",", 
            row.names = FALSE, na = "", col.names = T)


#### Demographics of data ####
str(Dat_cda)
summary(Dat_cda)

dens = ggplot(melt(Dat_cda), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.4, aes(fill = "red", col = "red"))+
  theme(legend.position="none")

plot(dens)

#### GGplot set up ####
# Not used at the moment, can be added as a layer to the plot
theme_setup =  theme(axis.text=element_text(size=20),
                     axis.title=element_text(size=22),
                     plot.title = element_text(size = 22),
                     strip.text = element_text(size = 20),
                     legend.position="bottom",
                     legend.text=element_text(size=20),
                     legend.title=element_blank(),     
                     panel.background = element_rect(fill = "lightgrey",
                                                     colour = "white", size = 4))


#### Correlation ####
# Correlation and p-value matrices
corrlist <- rcorr(as.matrix(Dat_cda[,-1]), type="pearson")
pvalues = data.frame(corrlist[["P"]])
correlation = round(data.frame(corrlist[["r"]]), digits = 3)

# Heatmap of the correlations
heatmap = ggcorr(Dat_cda[-1], 
                 low = "darkblue", mid = "lightyellow", high = "red",
                 label = T, label_size = 2.5, label_round = 3,   
                 color = 'black', size = 4, layout.exp = 2, hjust = 1) +
                 ggtitle('Correlation between explanatory & respons variables ')

plot(heatmap)

# Two strong correlations are further explored
scatterplot(Dat_cda$High_edu_perc, Dat_cda$Mean_income) # linear relationship




#### Normality ####
qqPlot(Dat_cda$Urban_index)
qqPlot(Dat_cda$High_edu_perc) # Amsterdam & Utrecht are deviant (~50% high educated)
qqPlot(Dat_cda$Mean_income) # Rozendaal & Bloemendaal are deviant
#qqPlot((Dat_cda$Non_west_perc)^(1/3)) # certainly not linear

hist(Dat_cda$High_edu_perc)




#### Boxplots ####
fill <- "#4271AE"
line <- "#1F3552"

# Plot Probability CDA votes VS Non-western residents
p1 <- ggplot(Dat_cda[complete.cases(Dat_cda),], aes(x = Non_west, y = CDA)) + 
       geom_boxplot(outlier.colour="black", 
                    outlier.size=2, 
                    outlier.fill =  "red",
                    na.rm = F,
                    fill = fill,
                    color = line) +
       ggtitle("Votes for CDA per municipality") +
       xlab("Percentage of non-western residents") +
       ylab("Probability") +
       scale_x_discrete(labels = c("< 5%", "5-10%", "> 10 %")) 

plot(p1)

# Plot Urbanity index VS Non-western residents
p2 <- ggplot(Dat_cda[complete.cases(Dat_cda),], 
             aes(x = Non_west, y = Urban_index)) + 
  geom_boxplot(outlier.colour="black", 
               outlier.size=2, 
               outlier.fill =  "red",
               na.rm = F,
               fill = fill,
               color = line) +
  ggtitle("Urbanity index against non-western residents") +
  xlab("Percentage of non-western residents") +
  ylab("Urbanity index") +
  ylim(c(0,4)) +
  scale_x_discrete(labels = c("< 5%", "5-10%", "> 10 %")) 
 

plot(p2)



#### Linear regression ####
rsq <- function (x, y) cor(x, y) ^ 2
rsq(Dat_cda$CDA, Dat_cda$Mean_income) # Calculate R-squared

# Linear regression between votes for CDA and mean income
Euro <- "\u20AC" # euro sign

p3 <- ggplot(Dat_cda, aes(x=Mean_income, y=CDA)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ggtitle("Probability of votes for CDA and mean income per municipality") +
  xlab(paste("Mean income per municipality (x ",Euro, " 1000)", sep = "")) +
  ylab("Probability of CDA votes") +
  xlim(c(20,42)) +
  annotate("rect", xmin = 35, xmax = 41, ymin = 0.32, ymax = 0.4, 
           fill="white", colour="red") +
  annotate("text", x=38, y=0.38, label = "correlation == -0.27", parse=T) + 
  annotate("text", x=38, y=0.35, label = "p-value < 0.001", parse=T)

plot(p3) # a few municipalities could be outliers -> possible high cooks dist?


p4 <- ggplot(Dat_cda, aes(x=Mean_income, y=High_edu_perc)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ggtitle("Highly educated residents and mean income per municipality ") +
  xlab(paste("Mean income per municipality (x ",Euro, " 1000)", sep = "")) +
  ylab("Probability of highly educated residents") +
  xlim(c(20,42)) +
  annotate("rect", xmin = 36.5, xmax = 42, ymin = 0.1, ymax = 0.18, 
           fill="white", colour="red") +
  annotate("text", x=39.5, y=0.16, label = "correlation == 0.567", parse=T) + 
  annotate("text", x=39.5, y=0.12, label = "p-value < 0.001", parse=T)

plot(p4)

# Urbanity index and highly educated residents
p5 <- ggplot(Dat_cda, aes(x=Urban_index, y=High_edu_perc)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ggtitle("Highly educated residents and urbanity index") +
  xlab("Urbanity index") +
  ylab("Probability of highly educated residents") +
  xlim(c(0,4)) +
  ylim(c(0,0.5)) + 
  annotate("rect", xmin = 3.0, xmax = 4.0, ymin = 0.0, ymax = 0.1, 
           fill="white", colour="red") +
  annotate("text", x=3.5, y=0.08, label = "correlation == 0.397", parse=T) + 
  annotate("text", x=3.5, y=0.04, label = "p-value < 0.001", parse=T)

plot(p5)


# Urbanity index and CDA votes
cor.test(Dat_cda$CDA, Dat_cda$Urban_index) # get correlation and p-val
p6 <- ggplot(Dat_cda, aes(x=Urban_index, y=CDA)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ggtitle("Probability of the votes for CDA and urbanity index per municipality") +
  xlab("Urbanity index") +
  ylab("Probability votes CDA") +
  xlim(c(0,4.1)) +
  #ylim(c(0,0.5)) + 
  annotate("rect", xmin = 2.95, xmax = 4.05, ymin = 0.33, ymax = 0.4, 
           fill="white", colour="red") +
  annotate("text", x=3.5, y=0.38, label = "correlation == -0.587", parse=T) + 
  annotate("text", x=3.5, y=0.35, label = "p-value < 0.001", parse=T)

plot(p6)

# Function to plot multiple plots together
multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multi_linear <- multiplot(p3, p6, p5, p4)
multi_boxpl <- multiplot(p1, p2)



#### Save plots ####
png('2_plots/Density_CDA.png', width = 15, height = 7, units='in',res=600)
print(dens)
dev.off()

png('2_plots/Heatmap_correlation_CDA.png', width = 10, height = 10, units='in',res=600)
print(heatmap)
dev.off()

png('2_plots/Linear_regression_CDA.png', width = 15, height = 7, units='in',res=600)
print(multi_linear)
dev.off()

png('2_plots/Boxplots_CDA.jpg', width = 15, height = 7, units='in',res=600)
print(multi_boxpl)
dev.off()



############################### End script #####################################