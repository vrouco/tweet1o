setwd("~/Dropbox/Victor/Cleaned_data_20-Oct")

es <- read.csv("es_db_indian.csv", sep=",", stringsAsFactors = F)
cat <- read.csv("ca_db_indian.csv", sep=",", stringsAsFactors = F)
eu2 <- read.csv("eu_db_indian.csv", sep=",", stringsAsFactors = F)
eu <- read.csv("euALL_db.csv", sep=";", stringsAsFactors = F)


#############################

########### FUNCTIONS & PACKAGES
require(devtools)
require(lubridate)#for dates
require(qdap)
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(cldr)
#demo(cldr)
###############


##################SELECT BASED ON LANGUAGE
language <- detectLanguage(eu$cleaned_text)
rowstokeep <- which(language$detectedLanguage == "ENGLISH")
eu <- eu[which(language$detectedLanguage == "ENGLISH"),]
eu$created <- strptime(eu$created,format= "%Y-%m-%d %H:%M:%S")
colnames(eu)[3] <- "text"
eu$text <- as.character(eu$text)
rm(eu2)

language <- detectLanguage(cat$cleaned_text)
rowstokeep <- which(language$detectedLanguage == "CATALAN")  ###WE COULD HAVE SELECTED CATALAN | SPANISH
cat <- cat[which(language$detectedLanguage == "CATALAN"),]
cat$created <- strptime(cat$created,format= "%Y-%m-%d %H:%M:%S")
colnames(cat)[4] <- "text"
cat$text <- as.character(cat$text)
which(duplicated(cat$text))
#x <- which(cat$lang=="es")
#cat <- cat[-x,]
cat2 <- cat[order(cat$text, -abs(as.numeric(cat$retweetCount)) ), ] 
y <- unique(cat2$text)
cat2 <- cat2[!duplicated(cat2$text),]
cat <- cat2
rm(cat2)

language <- detectLanguage(es$cleaned_text)
rowstokeep <- which(language$detectedLanguage == "SPANISH")
es <- es[which(language$detectedLanguage == "SPANISH"),]
es$created <- strptime(es$created,format= "%Y-%m-%d %H:%M:%S")
colnames(es)[4] <- "text"
es$text <- as.character(es$text)
x <- which(es$lang=="es")
es <- es[x,]
es2 <- es[order(es$text, -abs(as.numeric(es$retweetCount)) ), ] 
y <- unique(es2$text)
es2 <- es2[!duplicated(es2$text),]
es <- es2
rm(es2)
#############################################
#############################################
#     MAKE 1-gram TIBBLES   #
#############################################
#############################################
require(xlsx)
nrc <- read.xlsx2("modifiedNrc_cleaning empty rows V3.xlsx", 1,encoding = "UTF-8",header = F)
nrc <- nrc[,-1:-3]
colnames(nrc) <- c("sentiment", "cat","es","eu" )

lex.eu <- tibble(tweet = seq_along(eu$text),
                  created = cut(eu$created, breaks = "hour"),
                  text = eu$text,
                  retweet = as.numeric(eu$retweetCount))%>%
                  unnest_tokens(word, text) %>%
                  inner_join(nrc, by=c("word" = "eu"))

lex.eu <- lex.eu[-which(is.na(lex.eu$created)),]
lex.eu$created<- as.POSIXct(strptime(lex.eu$created,format= "%Y-%m-%d %H:%M:%S"))
hist(lex.eu$created, breaks="days")
lex.eu<- subset(lex.eu, lex.eu$created >= as.POSIXct('2017-10-01 06:00'))
lex.cat<- subset(lex.cat, lex.cat$created < as.POSIXct('2017-10-22 00:00'))
lex.eu$logretweet <- log1p(lex.eu$retweet)


lex.es <- tibble(tweet = seq_along(es$text),
                 created = cut(es$created, breaks = "hour"),
                 text = es$text,
                 retweet = as.numeric(es$retweetCount))%>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by=c("word" = "es"))

lex.es <- lex.es[-which(is.na(lex.es$created)),]
lex.es$created<- as.POSIXct(strptime(lex.es$created,format= "%Y-%m-%d %H:%M:%S"))
hist(lex.es$created, breaks="days")
lex.es<- subset(lex.es, lex.es$created >= as.POSIXct('2017-10-01 06:00'))
lex.es<- subset(lex.es, lex.es$created < as.POSIXct('2017-10-22 00:00'))
lex.es$logretweet <- log1p(lex.es$retweet)



lex.cat <- tibble(tweet = seq_along(cat$text),
                 created = cut(cat$created, breaks = "hour"),
                 text = cat$text,
                 retweet = as.numeric(cat$retweetCount))%>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by=c("word" = "cat"))

lex.cat <- lex.cat[-which(is.na(lex.cat$created)),]
lex.cat$created<- as.POSIXct(strptime(lex.cat$created,format= "%Y-%m-%d %H:%M:%S"))
hist(lex.cat$created, breaks="days")
lex.cat<- subset(lex.cat, lex.cat$created >= as.POSIXct('2017-10-01 06:00'))
lex.cat<- subset(lex.cat, lex.cat$created < as.POSIXct('2017-10-22 00:00'))
lex.cat$logretweet <- log1p(lex.cat$retweet)
#############################################
#############################################
#     MILESTONES   #
#############################################
#############################################
hitos <- read.csv("hitos.csv", sep=",")
colnames(hitos)<-c("X","created", "hito")
hitos$created<- as.POSIXct(strptime(hitos$created,format= "%Y-%m-%d %H:%M:%S"))
hitos<- subset(hitos, hitos$created < as.POSIXct('2017-10-22 00:00'))
#############################################
#############################################
#     EXPLORATORY   #
#############################################
#############################################
#preguntas a la bdd
#1)Están las emociones bien definidas?
#
#en refine 


write.csv(lex.cat, "data$lexicon cat.csv")
write.csv(lex.es, "data$lexicon es.csv")
write.csv(lex.eu, "data$lexicon eu.csv")

