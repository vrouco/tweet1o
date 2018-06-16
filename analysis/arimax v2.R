# set working directory

library("here")

# load required libraries
library('forecast')
library('tseries')
library('gridExtra')
library('TSA')
library('imputeTS')
library('mgcv')


here()
# load data
#cat = read.csv2("cat ts with val-ar from model.csv", sep=";") # catalonia

setwd(here("data/participants/time series data"))

cat = read.csv("cat nrc ts.csv", sep=",") # catalonia

setwd(here("data/milestones"))
shocks = read.csv2("shocks.csv", sep=','); shocks[1:3,3] = 0 # events
cat$index <- strptime(cat$index,format= "%Y-%m-%d %H:%M:%S")
cat <- subset(cat, cat$index < "2017-10-18 00:00:00")

zscore <- function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm = TRUE)
  for (i in 3:10) {
  cat[,i] <- zscore(cat[,i])
  }

# filter down outliers
for (i in 3:10) {
  cat[[i]] = forecast::tsclean(cat[[i]])
}

# filter out seasonality
ts_cat = data.frame(matrix(ncol = 8, nrow = nrow(cat)))
colnames(ts_cat) = colnames(cat[3:10])
cm_cat = list()
for (i in 3:10) {
  ts_cat[i-2] = ts(na.omit(cat[[i]]), frequency=12)
  decomp = stl(ts_cat[[i-2]], s.window="periodic")
  cm_cat[[i-2]] = decomp
  ts_cat[[i-2]] <- seasadj(decomp)
}

names(cm_cat) = colnames(cat[,3:10])
for (i in 1:length(cm_cat)) {
  plot(cm_cat[[i]], main=names(cm_cat[i]))
  
}

# check stationarity
adf.test(ts_cat$anger, alternative = "stationary") # stationarity test
Acf(ts_cat$arousal, main='ACF') # autocorrelation plot
Pacf(ts_cat$arousal, main='PACF') # parcial autocorrelation plot

# check stationarity if differencing is required
adf.test(diff(ts_cat$anger, differences = 1), alternative = "stationary")
Acf(diff(ts_cat$anger, differences = 1), main='ACF for Differenced Series')
Pacf(diff(ts_cat$anger, differences = 1), main='PACF for Differenced Series')

# fit arima model
fit1 = auto.arima(ts_cat$anger, stationary=TRUE, seasonal=FALSE, ic='aic', max.order=15)
fit1
tsdisplay(residuals(fit1), lag.max=15, main='Seasonal Model Residuals') # check residuals

# stepwise arimax
step <- list() # all possible combinations
for (i in 1:length(shocks[,3:11])) {
  step <- c(step, combn(shocks[,3:11],i,simplify=FALSE))
}
# THIS TAKES TOO LONG! MODEL FIND
# models <- list() # run models
# for (i in 1:length(step)) {
#   models[[i]] <- arimax(ts_cat$anger, order=c(1,0,1), xtransf=step[[i]], 
#                         transfer=rep(list(c(1,0)),length(step[[i]])), method='ML',
#                         optim.control = list(maxit = 1000))
# }

# aics <- as.numeric() # get AICs
# for (i in 1:length(models)) {
#   aics[i] <- models[[i]]$aic
# }
# min <- which(aics == min(aics)) # find lowest AIC model
# models[[min]]$aic < fit1$aic # check if the model is better than no intervention at all

# best models
#need to unrequire dplyr
fit.anger <- arimax(ts_cat$anger, order=c(1,0,1), xtransf=step[[18]],
                    transfer=transfers <- rep(list(c(1,0)),length(step[[18]])), 
                    method='ML', optim.control = list(maxit = 1000))

fit.anticipation <- arimax(ts_cat$anticipation, order=c(1,0,1), xtransf=step[[251]], 
                           transfer=transfers <- rep(list(c(1,0)),length(step[[251]])), 
                           method='ML', optim.control = list(maxit = 1000))
fit.disgust <- arimax(ts_cat$disgust, order=c(1,0,0), xtransf=step[[8]], 
                           transfer=transfers <- rep(list(c(1,0)),length(step[[8]])), 
                           method='ML', optim.control = list(maxit = 1000))
fit.fear <- arimax(ts_cat$fear, order=c(1,0,0), xtransf=step[[2]], 
                      transfer=transfers <- rep(list(c(1,0)),length(step[[2]])), 
                      method='ML', optim.control = list(maxit = 1000))
fit.joy <- arimax(ts_cat$joy, order=c(2,0,0), xtransf=step[[8]], 
                   transfer=transfers <- rep(list(c(1,0)),length(step[[8]])), 
                   method='ML', optim.control = list(maxit = 1000))
fit.sadness <- arimax(ts_cat$sadness, order=c(1,0,1), xtransf=step[[5]], 
                  transfer=transfers <- rep(list(c(1,0)),length(step[[5]])), 
                  method='ML', optim.control = list(maxit = 1000))
fit.surprise <- arimax(ts_cat$surprise, order=c(1,0,1), xtransf=step[[38]], 
                      transfer=transfers <- rep(list(c(1,0)),length(step[[38]])), 
                      method='ML', optim.control = list(maxit = 1000))
fit.trust <- arimax(ts_cat$trust, order=c(1,0,1), xtransf=step[[19]], 
                       transfer=transfers <- rep(list(c(1,0)),length(step[[19]])), 
                       method='ML', optim.control = list(maxit = 1000))
fit.valence <- arimax(ts_cat$valence, order=c(1,0,1), xtransf=step[[5]], 
                    transfer=transfers <- rep(list(c(1,0)),length(step[[5]])), 
                    method='ML', optim.control = list(maxit = 1000))
fit.arousal <- arimax(ts_cat$arousal, order=c(1,0,1), xtransf=step[[5]], 
                      transfer=transfers <- rep(list(c(1,0)),length(step[[5]])), 
                      method='ML', optim.control = list(maxit = 1000))
