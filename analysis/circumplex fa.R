es <- read.csv("spain nrc ts.csv")
cat <- read.csv("cat nrc ts.csv")
myts <- read.csv("europe nrc ts.csv")

#iberia <- rbind(myts, cat)
require(zoo)

#####to trim time

myts$index <- strptime(myts$index,format= "%Y-%m-%d %H:%M:%S")
myts <- subset(myts, myts$index < "2017-10-18 00:00:00")
 
myts[,3:10] <- as.data.frame(lapply(myts[,3:10], ts, start=1, frequency = 24))
myts[,3:10] <- as.data.frame(lapply(myts[,3:10], na.locf))
require(forecast)

#myts[,3:10] <- as.data.frame(lapply(myts[,3:10], tsclean))
#myts[,3:10] <- as.data.frame(lapply(myts[,3:10], ma, order=1))
myts[,3:10] <- as.data.frame(lapply(myts[,3:10], as.numeric))
lapply(myts, hist)
z.score <- function(x) (x-mean(x, na.rm=T))/sd(x, na.rm=T)

for(i in 3:10){
myts[,i] <- z.score(myts[,i])
}

##special for myts
myts[,3:10] <- as.data.frame(lapply(myts[,3:10], ts, start=1, frequency = 24))
trends <- as.data.frame(matrix(ncol=8, nrow=length(myts[,1])))
for(i in 1:8){
trends[,i]<- as.data.frame(stl(myts[,2+i], "periodic")[1])[,2]}

myts <- myts[,3:10]


myts <- as.data.frame(lapply(myts, log1p))
myts <- as.data.frame(lapply(myts, na.locf))
myts <- as.data.frame(lapply(myts, as.numeric))

factanal(myts, 3, rotation="varimax") #exploratorio

#confirmatorio
require(lavaan)
circumplex3 <- "valence =~ anger + joy + fear
                arousal =~ surprise + sadness"

fit <- cfa(circumplex3, myts,estimator = "MLM",std.lv = TRUE)

summary(fit, fit.measures=TRUE)
trial <- modificationindices(fit)
trial[order(trial$mi, decreasing = T),]
fit <- as.data.frame(lavPredict(fit, method="Bartlett"))
myts[,9:10]<-fit

write.csv2(myts, "cat ts with val-ar from model.csv")
