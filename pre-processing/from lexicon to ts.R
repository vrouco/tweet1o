setwd("~/Dropbox/Tweets 1-O")

############ DATA. WITH LEXICON

es <- read.csv("data$lexicon es.csv")
#es <- as.data.frame(es[es$sentiment!="anticipation",])
#es$sentiment <- droplevels(es$sentiment)

eu <- read.csv("data$lexicon eu.csv")
#eu <- as.data.frame(eu[eu$sentiment!="anticipation",])
#eu$sentiment <- droplevels(eu$sentiment)

cat <- read.csv("data$lexicon cat.csv")
#cat <- as.data.frame(cat[cat$sentiment!="anticipation",])
#cat$sentiment <- droplevels(cat$sentiment)

########### FUNCTIONS
require(devtools)
require(lubridate)#for dates
require(qdap)
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(cldr)
#demo(cldr)


##################################################################################################
##########################################        PLOT           #################################
##################################################################################################
require(tm)

  es2 <- es
  es2$created <- strptime(es$created,format= "%Y-%m-%d %H:%M:%S")
  es2$created <- as.POSIXct(es2$created) 
  es_ts <- es2 %>%
    dplyr::count(sentiment, index = created, wt = log1p(retweet))%>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise + anticipation),
           joy=(joy/n), trust=(trust/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
           anticipation=(anticipation/n),
           sadness=sadness/n, disgust=disgust/n)%>%
    ungroup()
  es_ts <- subset(es_ts, es_ts$index >= as.POSIXct('2017-10-01 09:00'))#Escojo las 9 porque hay dos horas vac´ías entre la anterior observaci´ón del dia 1 (6:00) y esta.
  require(zoo)
  es_ts[,2:10] <- as.data.frame(lapply(es_ts[2:10], ts,start=1, frequency=24))
  
  
  cat2 <- cat
  cat2$created <- strptime(cat$created,format= "%Y-%m-%d %H:%M:%S")
  cat2$created <- as.POSIXct(cat2$created) 
  cat_ts <- cat2 %>%
    dplyr::count(sentiment, index = created, wt = log1p(retweet))%>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise + anticipation),
           joy=(joy/n), trust=(trust/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
           sadness=sadness/n, anticipation=(anticipation/n), disgust=disgust/n)%>%
    ungroup()
  cat_ts<- subset(cat_ts, cat_ts$index >= as.POSIXct('2017-10-01 09:00'))
  require(zoo)
  cat_ts[,2:10] <- as.data.frame(lapply(cat_ts[2:10], ts,start=1, frequency=24))

  
  eu2 <- eu
  eu2$created <- strptime(eu$created,format= "%Y-%m-%d %H:%M:%S")
  eu2$created <- as.POSIXct(eu2$created) 
  eu_ts <- eu2 %>%
    dplyr::count(sentiment, index = created, wt = log1p(retweet))%>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise + anticipation),
           joy=(joy/n), trust=(trust/n), anticipation=(anticipation/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
           sadness=sadness/n, disgust=disgust/n)%>%
    ungroup()
  eu_ts<- subset(eu_ts, eu_ts$index >= as.POSIXct('2017-10-01 09:00'))
  require(zoo)
  eu_ts[,2:10] <- as.data.frame(lapply(eu_ts[2:10], ts,start=1, frequency=24))

  
  write.csv(eu_ts, "europe nrc ts.csv")
  write.csv(es_ts, "spain nrc ts.csv")
  write.csv(cat_ts, "cat nrc ts.csv")
  ##################################################################################################
  ##########################################        MILESTONES     #################################
  ##################################################################################################
  
  #setwd("~/Universitat de Barcelona/Alejandro Garcia Gutierrez - 1O/Victor")
  hitos <- read.csv("hitos V2.csv", sep=";", header=F, fileEncoding = "UTF-8")
  #hitos <- hitos[1:14, 2:3]
  colnames(hitos)<-c("created", "hito")
  hitos$created <- as.character(hitos$created)
  hitos$created <- as.POSIXlt(hitos$created, format = "%d/%m/%y %H:%M")
  hitos<-hitos[order(hitos$created),]
  hitos$id <- seq_along(hitos$created)
  #hitos <- hitos[-1,]
  hitos<- subset(hitos, hitos$created < as.POSIXct('2017-10-22 00:00'))

  
  
  
  
  milestones_intervention <- function(x){
    #setwd("~/Universitat de Barcelona/Alejandro Garcia Gutierrez - 1O/Victor")
    hitos <- read.csv("hitos V2.csv", sep=";", header=F, fileEncoding = "UTF-8")
    #hitos <- hitos[1:14, 2:3]
    colnames(hitos)<-c("created", "hito")
    hitos$created <- as.character(hitos$created)
    hitos$created <- as.POSIXlt(hitos$created, format = "%d/%m/%y %H:%M")
    hitos<-hitos[order(hitos$created),]
    hitos$id <- seq_along(hitos$created)
    #hitos <- hitos[-1,]
    hitos<- subset(hitos, hitos$created < as.POSIXct('2017-10-22 00:00'))
    
    
  hitos_ts <- as.data.frame(matrix(NA,ncol=length(hitos$hito), nrow=length(x$index))) #cargar el plot.es de abajo antes si no va
  for(i in 1:dim(hitos_ts)[2]){
    hitos_ts[,i]<-c(rep(0, which(as.POSIXlt(x$index) %in% hitos$created[i])-1),
                    rep(1,times= length(hitos_ts[,1])-which(as.POSIXlt(x$index) %in% hitos$created[i])+1))
  }
  colnames(hitos_ts)<-gsub("([A-Za-z]+).*", "\\1", hitos$hito)
  colnames(hitos_ts)[c(5,6,7,8)]<-c("Organizations","Dem.dialogue", "Dem.Unity", "FirstDOI")
  return(hitos_ts)
  }
  
  trial <- milestones_intervention(es_ts)
  
  milestones_regression <- function(x, y=0){ #numberOfLagsAfter = y, numberOfLagsBefore = z
    
     hitos <- read.csv("hitos V2.csv", sep=";", header=F, fileEncoding = "UTF-8")
     colnames(hitos)<-c("created", "hito")
     hitos$created <- as.character(hitos$created)
     hitos$created <- as.POSIXlt(hitos$created, format = "%d/%m/%y %H:%M")
     hitos<-hitos[order(hitos$created),]
     hitos$id <- seq_along(hitos$created)
     hitos<- subset(hitos, hitos$created < as.POSIXct('2017-10-21 00:00'))
    
    hitos_ts <- as.data.frame(matrix(NA,ncol=length(hitos$hito), nrow=length(x$index))) 
    for(i in 1:dim(hitos_ts)[2]){
      hitos_ts[,i]<-c(rep(0, which(as.POSIXlt(x$index) %in% hitos$created[i])+1),
                      rep(1, times = y),
                      rep(0, times = abs(length(hitos_ts[,i]) - (y+(which(as.POSIXlt(x$index) %in% hitos$created[i]))-1))))
      }
    colnames(hitos_ts)<-gsub("([A-Za-z]+).*", "\\1",hitos$hito)
    colnames(hitos_ts)[c(5,6,7,8)]<-c("Organizations","Dem.dialogue", "Dem.Unity", "FirstDOI")
    return(hitos_ts)
  }
  
  
  trial <- milestones_regression(es_ts, y=8)
  
  
  