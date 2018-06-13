es <- read.csv("spain nrc ts.csv")




trial <- milestones_regression(es,8,1)  #funcion creada en from lexicon to ts
es <- cbind(es, trial)
es[12:22] <- as.data.frame(lapply(es[12:22], ts,start=1, frequency=24))


require(astsa)
require(forecast)
require(TSA)
require(zoo)

par(mfrow=c(4,2), mar=c(0,0,0,0))
for(i in 3:10){
plot(tsclean(es[,i]), main=colnames(es)[i])}


trash <- lm(anger~Police+Strike+Kings+Puigdemont+Organizations+Dem.dialogue+Dem.Unity+FirstDOI+Referendum+Ultimatum+Article, es)
