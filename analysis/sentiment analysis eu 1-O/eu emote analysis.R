setwd("~/Documents/GitHub/tweet1o/sentiment analysis eu 1-O")


eu <- read.csv("europe1-0.csv", sep=";", encoding="latin1")

require(devtools)
devtools::install_version("cldr",version="1.1.0")#language detection
library(cldr)
#demo(cldr)

language <- detectLanguage(eu$text)

arrayoflang <- language[which(language$detectedLanguage!="SPANISH" & language$detectedLanguage!="CATALAN"), 1]
arrays <- table(arrayoflang)
arrays.prop <- prop.table(arrays)
arrays[order(as.numeric(arrays), decreasing = T)]
arrays.prop[order(as.numeric(arrays.prop), decreasing = T)] #we're ok to select only english as it represents 91.4% of the non spanish-non catalan

howmany <- language[which(language$detectedLanguage=="SPANISH" | language$detectedLanguage=="CATALAN"), 1]
table(howmany)#773 CAT; 1539 ESP. Igual estaba bien hacer análisis de estos dos por separado

rowstokeep <- which(language$detectedLanguage == "ENGLISH")

eu <- eu[which(language$detectedLanguage == "ENGLISH"),]


###############################################ANALYSIS

require(readxl)
emote <- read_excel("EMOTE.xlsx", 1)
emote$word
eu$text <- as.ch3waracter(eu$text)
splittext <- strsplit(eu$text, split=" ")

for(i in 1:length(splittext)){
    print(which(splittext[[4342]] %in% emote$word))#atencion aquí el texto es muy grande. casualmente es la ultima fila
}
  
  