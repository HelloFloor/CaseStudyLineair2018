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

rm(list = ls()) # empty work space

#### Libraries ####
library(car)
library(tidyr)
library(GGally)
library(Hmisc)
library(lattice)
library(survival)

#### Import data ####

Dat2 <- read.csv("1_clean_data/Clean_data_all_variables_2018-12-21.csv",
                 stringsAsFactors=F, header = T)



#### Demographics of data ####
Dat2$Non_west <- as.factor(Dat2$Non_west)
summary(Dat2) # zet in verslag & presentatie. Leg uit of variabele numeriek/factor is

dens = ggplot(melt(Dat2), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) + 
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.4, aes(fill = "red", col = "red"))+
  theme(legend.position="none")

plot(dens) # zet in verslag: normaal verdeling, 
# density bij CDA/GL/Perc 60plus/High edu
# is boven de 1. Dit komt omdat de x-as kleine waarden heeft. 
# Area onder curve somt tot 1



#### Correlation ####
# Correlation and p-value matrices
# Do not put in report
corrlist <- rcorr(as.matrix(Dat2[,-1]), type="pearson")
pvalues = data.frame(corrlist[["P"]])
correlation = round(data.frame(corrlist[["r"]]), digits = 3)

# Heatmap of the correlations
# Zet in verslag: leg correlaties uit of we dat verwachten/hoe te verklaren
heatmap = ggcorr(Dat2[-1], 
                 low = "darkblue", mid = "lightyellow", high = "red",
                 label = T, label_size = 2.5, label_round = 3,   
                 color = 'black', size = 4, layout.exp = 2, hjust = 1) 
                 #ggtitle('Correlation between explanatory & respons variables ')

plot(heatmap)



#### Boxplots ####
# For report: explain how to interpretate a boxplot
# Plot Probability CDA votes VS Non-western residents
p1 <- ggplot(Dat2, aes(x = Non_west, y = CDA_frac, fill = Non_west)) + 
       geom_boxplot(outlier.colour="black", 
                    outlier.size=2, 
                    outlier.fill =  "red",
                    na.rm = F,
                    color = "black") +
       #ggtitle("Votes for CDA per municipality") +
       xlab("Non-western residents") +
       ylab("Votes for CDA") +
       scale_x_discrete(labels = c("< 5%", "5-10%", "> 10 %")) +
       guides(fill = F)

plot(p1)

# Plot Urbanity index VS Non-western residents
p2 <- ggplot(Dat2, 
             aes(x = Non_west, y = Urban_index, fill = Non_west)) + 
  geom_boxplot(outlier.colour="black", 
               outlier.size=2, 
               outlier.fill =  "red",
               na.rm = F,
               color ="black") +
  #ggtitle("Urbanity index against non-western residents") +
  xlab("Non-western residents") +
  ylab("Urbanity index") +
  ylim(c(0,4)) +
  scale_x_discrete(labels = c("< 5%", "5-10%", "> 10 %")) +
  guides(fill = F)
 

plot(p2)

# GL against Non_west
p2a <- ggplot(Dat2, aes(x = Non_west, y = GL_frac, fill = Non_west)) + 
  geom_boxplot(outlier.colour="black", 
               outlier.size=2, 
               outlier.fill =  "red",
               na.rm = F,
               color = "black") +
  #ggtitle("Votes for CDA per municipality") +
  xlab("Non-western residents") +
  ylab("Votes for GL") +
  scale_x_discrete(labels = c("< 5%", "5-10%", "> 10 %")) +
  guides(fill = F)

plot(p2a)


#### Linear regression ####
# Report: explain what is on the x and y-axis
# We can report the R-squared in the report (but we do not need to show this function)
rsq <- function (x, y) cor(x, y) ^ 2
rsq(Dat2$CDA_frac, Dat2$Mean_income) # Calculate R-squared

# Linear regression between votes for CDA and mean income
Euro <- "\u20AC" # euro sign

p3 <- ggplot(Dat2, aes(x=Mean_income, y=CDA_frac)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Probability of votes for CDA and mean income per municipality") +
  xlab(paste("Mean income per municipality (x ",Euro, " 1000)", sep = "")) +
  ylab("Votes for CDA") +
  ylim(c(0,0.45)) +
  xlim(c(20,42)) #+
  #annotate("rect", xmin = 35, xmax = 41, ymin = 0.32, ymax = 0.4, 
   #        fill="white", colour="red") +
  #annotate("text", x=38, y=0.38, label = "correlation == -0.27", parse=T) + 
  #annotate("text", x=38, y=0.35, label = "p-value < 0.001", parse=T)

plot(p3) 


p4 <- ggplot(Dat2, aes(x=Mean_income, y=High_educated_frac)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Highly educated residents and mean income per municipality ") +
  xlab(paste("Mean income (x ",Euro, " 1000)", sep = "")) +
  ylab("Highly educated residents") +
  xlim(c(20,42)) #+
  #annotate("rect", xmin = 36.5, xmax = 42, ymin = 0.1, ymax = 0.18, 
   #        fill="white", colour="red") +
  #annotate("text", x=39.5, y=0.16, label = "correlation == 0.567", parse=T) + 
  #annotate("text", x=39.5, y=0.12, label = "p-value < 0.001", parse=T)

plot(p4)

# Urbanity index and highly educated residents
p5 <- ggplot(Dat2, aes(x=Urban_index, y=High_educated_frac)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Highly educated residents and urbanity index") +
  xlab("Urbanity index") +
  ylab("Highly educated residents") +
  xlim(c(0,4))
  #ylim(c(0,0.53))# + 
  #annotate("rect", xmin = 3.0, xmax = 4.0, ymin = 0.0, ymax = 0.1, 
      #     fill="white", colour="red") +
  #annotate("text", x=3.5, y=0.08, label = "correlation == 0.397", parse=T) + 
  #annotate("text", x=3.5, y=0.04, label = "p-value < 0.001", parse=T)

plot(p5)


# Urbanity index and CDA votes
p6 <- ggplot(Dat2, aes(x=Urban_index, y=CDA_frac)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Probability of the votes for CDA and urbanity index per municipality") +
  xlab("Urbanity index") +
  ylab("Votes for CDA") +
  xlim(c(0,4.1)) +
  ylim(c(0,0.45))# + 
  #annotate("rect", xmin = 2.95, xmax = 4.05, ymin = 0.33, ymax = 0.4, 
      #     fill="white", colour="red") +
  #annotate("text", x=3.5, y=0.38, label = "correlation == -0.587", parse=T) + 
  #annotate("text", x=3.5, y=0.35, label = "p-value < 0.001", parse=T)

plot(p6)

# Urbanity index and GL votes
p7 <- ggplot(Dat2, aes(x=Urban_index, y=GL_frac)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Probability of the votes for CDA and urbanity index per municipality") +
  xlab("Urbanity index") +
  ylab("Votes for GL") +
  xlim(c(0,4)) +
  ylim(c(0,0.25))# + 
#annotate("rect", xmin = 2.95, xmax = 4.05, ymin = 0.33, ymax = 0.4, 
#     fill="white", colour="red") +
#annotate("text", x=3.5, y=0.38, label = "correlation == -0.587", parse=T) + 
#annotate("text", x=3.5, y=0.35, label = "p-value < 0.001", parse=T)

plot(p7)

# Highly educted residents and GL votes
p8 <- ggplot(Dat2, aes(x=High_educated_frac, y=GL_frac)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Probability of the votes for CDA and urbanity index per municipality") +
  xlab("Highly educated residents") +
  ylab("Votes for GL") +
  #xlim(c(0, 0.6)) +
  ylim(c(0,0.25))# + 
#annotate("rect", xmin = 2.95, xmax = 4.05, ymin = 0.33, ymax = 0.4, 
#     fill="white", colour="red") +
#annotate("text", x=3.5, y=0.38, label = "correlation == -0.587", parse=T) + 
#annotate("text", x=3.5, y=0.35, label = "p-value < 0.001", parse=T)

plot(p8)


# Urbanity index and GL votes
p9 <- ggplot(Dat2, aes(x=Urban_index, y=Frac_60plus)) + 
  geom_point() + 
  geom_smooth(method=lm, se = T) + # standard error = True
  ##ggtitle("Probability of the votes for CDA and urbanity index per municipality") +
  xlab("Urbanity index") +
  ylab("> 60 years old residents") +
  xlim(c(0, 4)) +
  ylim(c(0,0.20))# + 
#annotate("rect", xmin = 2.95, xmax = 4.05, ymin = 0.33, ymax = 0.4, 
#     fill="white", colour="red") +
#annotate("text", x=3.5, y=0.38, label = "correlation == -0.587", parse=T) + 
#annotate("text", x=3.5, y=0.35, label = "p-value < 0.001", parse=T)

plot(p9)

# Function to plot multiple plots together
multiplot <- function(..., plotlist=NULL, file, cols= 3, layout=NULL) {
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

multi_linearCDA <- multiplot(p3, p6, cols = 2)
multi_linear <- multiplot(p4, p5, p9, cols = 3)
multi_boxpl <- multiplot(p1, p2a, p2, cols = 3)
multi_linearGL <- multiplot(p7, p8, cols = 2)


#### Save plots ####
png('Plots/Visualisation_Density.png', width = 15, height = 7, units='in',res=600)
print(dens)
dev.off()

png('Plots/Visualisation_Heatmap.png', width = 10, height = 10, units='in',res=600)
print(heatmap)
dev.off()

png('Plots/Visualisation_LinearCDA.png', width = 15, height = 7, units='in',res=600)
plot(multiplot(p3, p6, cols = 2))
dev.off()



############################### End script #####################################