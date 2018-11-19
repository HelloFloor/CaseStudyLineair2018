setwd("C:/Users/HP/Documents/Master Statistical Science & Data Science/02 - Linear Models/Case Study")


####Maak een data frame voor stedelijkheid
raw_data <- read.csv("0_data/stedelijkheid.csv",sep=";")

data <- raw_data
data <- data[-389,]
data$Gemeente <- as.character(data$Gemeente)

#Maak van de vijf categorieën een mooi indexnummer
stedelijkheidsindex <- 
  (4*data$Zeer.sterk.stedelijk + 3*data$Sterk.stedelijk + 2*data$Matig.stedelijk + 1*data$Weinig.stedelijk) /
  (data$Zeer.sterk.stedelijk+data$Sterk.stedelijk+data$Matig.stedelijk+data$Weinig.stedelijk+data$Niet.stedelijk)

data <- cbind(data,stedelijkheidsindex)





####Maak een data frame voor de uitslagen

uitslag <- read.csv("0_data/uitslag.csv",sep=";")

#verwijder partijen die geen zetel gehaald hebben
uitslag <- uitslag[,1:15]
colnames(uitslag) <- c("Gemeente", "GemeenteCode", "VVD","CDA","PVV", "D66", "SP", "GL","PvdA","CU","50+","PvdD","SGP","FvD","DENK")

#characters leuker dan factors
uitslag$Gemeente <- as.character(uitslag$Gemeente)

#op alfabet sorteren, analoog aan de stedelijkheidsdata
uitslag <- uitslag[order(uitslag$Gemeente),]




####Intermezzo: Welke gemeentes hebben verschillende namen?
#Zie ook gemeentenamenchecker.r

#verwijder de carabische eilanden, ze hebben geen stedelijkheidsdata
uitslag <- uitslag[!(uitslag$Gemeente %in% c("Bonaire","Saba","Sint Eustatius")),]

#Pas her en der wat namen aan
data$Gemeente[data$Gemeente=="Utrecht (gemeente)"] <- "Utrecht"
data$Gemeente[data$Gemeente=="Groningen (gemeente)"] <- "Groningen"
data$Gemeente[data$Gemeente=="'s-Gravenhage (gemeente)"] <- "'s-Gravenhage"
data$Gemeente[data$Gemeente=="Middelburg (Z.)"] <- "Middelburg"
data$Gemeente[data$Gemeente=="Beek (L.)"] <- "Beek"
data$Gemeente[data$Gemeente=="Bergen (L.)"] <- "Bergen (L)"
data$Gemeente[data$Gemeente=="Bergen (NH.)"] <- "Bergen (NH)"
data$Gemeente[data$Gemeente=="Hengelo (O.)"] <- "Hengelo"
data$Gemeente[data$Gemeente=="Laren (NH.)"] <- "Laren"
data$Gemeente[data$Gemeente=="Rijswijk (ZH.)"] <- "Rijswijk"
data$Gemeente[data$Gemeente=="Stein (L.)"] <- "Stein"
uitslag$Gemeente[uitslag$Gemeente=="SÃºdwest-FryslÃ¢n"] <- "Súdwest-Fryslân"

#Verwijder Boxmeer, omdat het de verkiezingsuitslag incompleet heeft doorgegeven. Foei!
uitslag <- uitslag[!(uitslag$Gemeente =="Boxmeer"),]
data <- data[!(data$Gemeente =="Boxmeer"),]




####Maak een nieuw dataframe om mee te werken
stem_sted <- data.frame(data$stedelijkheidsindex)
rownames(stem_sted) <- data$Gemeente
rownames(data) <- data$Gemeente
rownames(uitslag) <- uitslag$Gemeente


politieke_partijen <- c("VVD","CDA","PVV", "D66", "SP", "GL","PvdA","CU","50+","PvdD","SGP","FvD","DENK")

#Voeg het totaal aantal stemmen aan partijen met zetels toe
for (plaats in rownames(stem_sted)){
stem_sted[plaats,"Totale stemmen"] <- sum(uitslag[plaats,politieke_partijen])
}

#geef percentages voor iedere partij
for (plaats in rownames(stem_sted)) {
  for(partij in politieke_partijen){
    stem_sted[plaats,partij] <-  uitslag[plaats,partij]/stem_sted[plaats,"Totale stemmen"]
  }
}

#maak een correlatievector
stem_sted_cors <- numeric(13)
names(stem_sted_cors) <- politieke_partijen
for (partij in politieke_partijen) {
  stem_sted_cors[partij] <- cor(stem_sted$data.stedelijkheidsindex,stem_sted[,partij])
}

print(stem_sted_cors)




####Maak een mooie grafiek
politieke_kleur <- c("darkblue","darkgreen","lightblue","lightgreen","tomato","forestgreen","orangered","yellow","purple","pink","gray50","brown","turquoise")
names(politieke_kleur) <- politieke_partijen

plot(c(0,4),c(0,0.3),type='n',xlab="Stedelijkheid",ylab = "Aandeel stemmen")

for (partij in politieke_partijen) {
  
abline(lm(stem_sted[,partij]~stem_sted$data.stedelijkheidsindex)$coefficients, col=politieke_kleur[partij])
  
} 
legend("topleft",politieke_partijen,col = politieke_kleur,lty = 2)


plot(stem_sted$data.stedelijkheidsindex, stem_sted$CDA)
partij="CDA"; abline(lm(stem_sted[,partij]~stem_sted$data.stedelijkheidsindex)$coefficients, col=politieke_kleur[partij])
