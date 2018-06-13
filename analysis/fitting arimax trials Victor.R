############ DATA. WITH LEXICON

es <- read.csv("data$lexicon es.csv")
es <- as.data.frame(es[es$sentiment!="anticipation",])
es$sentiment <- droplevels(es$sentiment)

eu <- read.csv("data$lexicon eu.csv")
eu <- as.data.frame(eu[eu$sentiment!="anticipation",])
eu$sentiment <- droplevels(eu$sentiment)

cat <- read.csv("data$lexicon cat.csv")
cat <- as.data.frame(cat[cat$sentiment!="anticipation",])
cat$sentiment <- droplevels(cat$sentiment)

########### FUNCTIONS
require(devtools)
require(lubridate)#for dates
require(qdap)
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(cldr)
#demo(cldr)


#setwd("~/Universitat de Barcelona/Alejandro Garcia Gutierrez - 1O/Victor")
hitos <- read.csv("hitos.csv", sep=",", fileEncoding = "UTF-8")
hitos <- hitos[1:14, 2:3]
colnames(hitos)<-c("created", "hito")
hitos$created <- as.POSIXct(hitos$created)
hitos<-hitos[order(hitos$created),]
hitos$id <- seq_along(hitos$created)
hitos <- hitos[-1,]
hitos<- subset(hitos, hitos$created < as.POSIXct('2017-10-22 00:00'))


hitos_ts <- as.data.frame(matrix(NA,ncol=length(hitos$hito), nrow=length(plot.es$index))) #cargar el plot.es de abajo antes si no va
for(i in 1:dim(hitos_ts)[2]){
  hitos_ts[,i]<-c(rep(0, which(plot.es$index %in% hitos$created[i])),
                  rep(1,times= length(hitos_ts[,1])-which(plot.es$index %in% hitos$created[i])))
}
colnames(hitos_ts)<-hitos$hito

##################################################################################################
##########################################        PLOT           #################################
##################################################################################################
require(tm)



#esto saca 7 plots, con lineas de color segun muestra. De la frecuencia de los sentimientos 
#ponderada
par(mfrow=c(2,4), mar=c(3,2,1,0))

for(i in 1:7){
  es2 <- es
  es2$created <- strptime(es$created,format= "%Y-%m-%d %H:%M:%S")
  es2$created <- as.POSIXct(es2$created) 
  plot.es <- es2 %>%
    dplyr::count(sentiment, index = created, wt = log1p(retweet))%>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise),
           joy=(joy/n), trust=(trust/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
           
           sadness=sadness/n, disgust=disgust/n)%>%
    ungroup()
  plot.es<- subset(plot.es, plot.es$index >= as.POSIXct('2017-10-01 06:00'))
  require(zoo)
  trialf <- na.locf(ts(plot.es[,c(1,1+i)], start=1, frequency = 24))
  trialf <- trialf[,2]
  trialf2 <- decompose(trialf)
  plot(trialf2[[3]], main=colnames(plot.es)[1+i], ylab="trend", xlab="",
       ylim=c(0,0.4), col="red")
  
  
  cat2 <- cat
  cat2$created <- strptime(cat$created,format= "%Y-%m-%d %H:%M:%S")
  cat2$created <- as.POSIXct(cat2$created) 
  plot.cat <- cat2 %>%
    dplyr::count(sentiment, index = created, wt = log1p(retweet))%>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise),
           joy=(joy/n), trust=(trust/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
           sadness=sadness/n, disgust=disgust/n)%>%
    ungroup()
  plot.cat<- subset(plot.cat, plot.cat$index >= as.POSIXct('2017-10-01 06:00'))
  trialf <- na.locf(ts(plot.cat[,c(1,1+i)], start=1, frequency = 24))
  trialf <- trialf[,2]
  trialf2 <- decompose(trialf)
  lines(trialf2[[3]], col="#BABA00")
  
  
  eu2 <- eu
  eu2$created <- strptime(eu$created,format= "%Y-%m-%d %H:%M:%S")
  eu2$created <- as.POSIXct(eu2$created) 
  plot.eu <- eu2 %>%
    dplyr::count(sentiment, index = created, wt = log1p(retweet))%>%
    ungroup() %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(n = (joy + trust + anger + fear + sadness + disgust + surprise),
           joy=(joy/n), trust=(trust/n), anger=(anger/n), fear=fear/n,surprise=surprise/n,
           sadness=sadness/n, disgust=disgust/n)%>%
    ungroup()
  plot.eu<- subset(plot.eu, plot.eu$index >= as.POSIXct('2017-10-01 06:00'))
  require(zoo)
  trialf <- na.locf(ts(plot.eu[,c(1,1+i)], start=1, frequency = 24))
  trialf <- trialf[,2]
  trialf2 <- decompose(trialf)
  lines(trialf2[[3]], col="blue")
}

#por qué trust tan alto en eu?
trial <- as.data.frame(count(eu[eu$sentiment=="trust",], word))
head(trial[order(trial$n, decreasing = T),], n=30)

trial2 <- as.data.frame(count(es[es$sentiment=="trust",], word))
head(trial2[order(trial2$n, decreasing = T),], n=30)


##################################################################################################
########################################## Intervention analysis / ARIMA #########################
##################################################################################################



##########################################
##########################################
shocks <- read.csv("hitos V2.csv", sep=";", header = F)
shocks$V1 = as.POSIXct(shocks$V1)
shocks = shocks[order(shocks$V1),]
#shocks <- subset(shocks, shocks$V1 < "2017/10/21 00:00:00")
shocks<-shocks[-14:-13,]

myts <- plot.cat
myts[(length(myts$index)):(length(myts$index)+nrow(shocks))] <- NA
colnames(myts)[11:18] <- as.character(shocks$V2)

for (i in 1:nrow(myts)) {
  for (j in 1:nrow(shocks)) {
    if (myts$index[i] < shocks$V1[j]) {
      myts[i,10+j] <- 0     
    }
    else {
      myts[i,10+j] <- 1
    }
  }
}

count_shocks <- as.matrix(ts(na.omit(ts_europe[11:18]), frequency=24))[1:259,]

##########################################
##########################################


require(astsa)
require(forecast)
require(TSA)
require(zoo)

angercat <-  na.locf(ts(es_ts$anger, start=1, frequency = 24))
ndiffs(angercat) #Number of differences required to achieve stationarity 
plot(diff(angercat, differences=1))
angercat2 <- diff(angercat, differences=1)
sarima(angercat2, 0,0,1)
sarima(angercat, 0,1,1) #son iguales.

trial <- sarima(angercat, 1,1,1,0,0,0,S=24)
plot(resid(trial$fit))

require(tsoutliers)
out <- tso(angercat)
plot.tsoutliers(out) #pero ver lo shitos. Los outliers son comprensibles a la luz de los hitos

arimax(angercat, order=c(1,1,1), xtransf=count_shocks, transfer=list(c(1,0)))




discat <-  na.locf(ts(plot.cat$surprise, start=1, frequency = 24))
ndiffs(discat) #Number of differences required to achieve stationarity 
plot(diff(discat, differences=1))
discat2 <- diff(discat, differences=1)
sarima(discat2, 0,0,1)
sarima(discat, 0,1,1) #son iguales.

trial <- sarima(discat, 2,1,2,1,0,0,S=24)
plot(resid(trial$fit))

require(tsoutliers)
out <- tso(discat)
plot.tsoutliers(out) #pero ver lo shitos. Los outliers son comprensibles a la luz 


#smoothing
#with moving averages
plot(ma(angercat, order = 24))
trial <- ma(angercat, order = 3)
#with decomposition, also uses moving avgs.
plot(decompose(trial, type="additive"))





















discat <- tsclean(discat)
discat <- na.locf(ma(tsclean(discat), order=1), na.rm=F)
discat[is.na(discat)]<-mean(discat[10:length(discat)-10])
acf2(discat)
sarima(angercat, 1,1,1)




sarima(xdata, p, d, q, P = 0, D = 0, Q = 0, S = -1,  details = TRUE, xreg=NULL, Model=TRUE, tol = sqrt(.Machine$double.eps),  no.constant = FALSE)
plot.cat <- as.data.frame(plot.cat)
angercat <-  na.locf(ts(plot.cat[,2], start=1, frequency = 24))
hitos_ts <- as.data.frame(matrix(NA,ncol=length(hitos$hito), nrow=length(plot.cat$index))) #cargar el plot.es de abajo antes si no va
for(i in 1:dim(hitos_ts)[2]){
  hitos_ts[,i]<-c(rep(0, which(plot.cat$index %in% hitos$created[i])),
                  rep(1,times= length(hitos_ts[,1])-which(plot.cat$index %in% hitos$created[i])))
}
colnames(hitos_ts)<-hitos$hito
adf.test(na.locf(ts(plot.cat[,2], start=1, frequency = 24)))
acf2(na.locf(ts(plot.cat[,2], start=1, frequency = 24)))


sarima(angercat, 2, 0, 0, P = 0, D = 0, Q = 0, S = -1, xreg=hitos_ts, Model=TRUE, tol = sqrt(.Machine$double.eps),  no.constant = F)
mylist <- rep(list(c(0,1)), 10)

require(forecast)
require(TSA)
na.locf(ma(tsclean(angercat), order=12))
myts <- na.locf(ma(tsclean(angercat), order=12), na.rm=F)
myts[1:6]<-0

decompose= stl(myts, s.window="periodic")

deseasonal_trial <- seasadj(decompose)
adf.test(deseasonal_trial)
acf2(deseasonal_trial)
arimax(deseasonal_trial, order = c(1,0,0), xtransf = as.matrix(hitos_ts), transfer = rep(list(c(0,1)), 10))

arimax(na.locf(ts(plot.cat[,3], start=1, frequency = 24)),)
