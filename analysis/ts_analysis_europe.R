setwd("~/Desktop/polarity")
library('ggplot2')
library('forecast')
library('tseries')
library('gridExtra')
library('TSA')

ts_europe <- read.csv("circumplex_europe.csv")
ts_europe$created = as.POSIXct(ts_europe$created, format = "%Y-%m-%d %H:%M:%S")
shocks <- read.csv("hitos.csv")
shocks$X.1 = as.POSIXct(shocks$X.1, format = "%Y-%m-%d %H:%M:%S")
shocks = shocks[order(shocks$X.1),]
shocks <- subset(shocks, shocks$X.1 >= "2017-10-01 00:00:00" & shocks$X.1 < "2017-10-16 00:00:00")

ts_europe[(length(ts_europe)):(length(ts_europe)+nrow(shocks))] <- NA
colnames(ts_europe)[11:18] <- as.character(shocks$X.2)

for (i in 1:nrow(ts_europe)) {
  for (j in 1:nrow(shocks)) {
    if (ts_europe$created[i] < shocks$X.1[j]) {
      ts_europe[i,10+j] <- 0     
    }
    else {
      ts_europe[i,10+j] <- 1
    }
  }
}

plot1 <- ggplot(data = ts_europe, aes(x=ts_europe$created, y=ts_europe$md_arousal)) + 
  geom_line(size = 0.3, color="#CC79A7") + ggtitle("Arousal") + xlab("Date") + ylab("polarity") # arousal ts

plot2 <- ggplot(data = ts_europe, aes(x=ts_europe$created, y=ts_europe$md_valence)) + 
  geom_line(size = 0.3, color="#0072B2") + ggtitle("Valence") + xlab("Date") + ylab("polarity") # valence ts

grid.arrange(plot1, plot2, nrow=2, ncol=1) # plot circumpflex

ts_europe$clean_aro = tsclean(ts_europe$md_arousal)
ts_europe$clean_val = tsclean(ts_europe$md_valence)

plot3 <- ggplot(data = ts_europe, aes(x=ts_europe$created, y=ts_europe$clean_aro)) + 
  geom_line(size = 0.3, color="#CC79A7") + ggtitle("Arousal") + xlab("Date") + ylab("polarity") # arousal ts

plot4 <- ggplot(data = ts_europe, aes(x=ts_europe$created, y=ts_europe$clean_val)) + 
  geom_line(size = 0.3, color="#0072B2") + ggtitle("Valence") + xlab("Date") + ylab("polarity") # valence ts

grid.arrange(plot3, plot4, nrow=2, ncol=1) # plot circumpflex

ts_europe$aro_ma = ma(ts_europe$clean_aro, order=24) # using the clean count with no outliers
ts_europe$val_ma = ma(ts_europe$clean_val, order=24) # using the clean count with no outliers

count_aro = ts(na.omit(ts_europe$aro_ma), frequency=24)
count_shocks <- as.matrix(ts(na.omit(ts_europe[11:18]), frequency=24))[1:259,]
decomp_aro = stl(count_aro, s.window="periodic")
deseasonal_aro <- seasadj(decomp_aro)
plot(decomp_aro)
adf.test(count_aro, alternative = "stationary")

count_val = ts(na.omit(ts_europe$val_ma), frequency=24)
decomp_val = stl(count_val, s.window="periodic")
deseasonal_val <- seasadj(decomp_val)
plot(decomp_val)
adf.test(count_val, alternative = "stationary")

model1 <- arimax(deseasonal_aro, order=c(1,1,1), xtransf=count_shocks, transfer=list(c(1,0)))
