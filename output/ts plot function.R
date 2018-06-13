library('ggplot2')
library('forecast')
library('tseries')
library('gridExtra')
library('TSA')
library('imputeTS')
library('mgcv')


cat = read.csv("cat nrc ts.csv", sep=",") # catalonia
shocks = read.csv2("shocks.csv", sep=','); shocks[1:3,3] = 0 # events
cat$index <- strptime(cat$index,format= "%Y-%m-%d %H:%M:%S")
cat <- subset(cat, cat$index < "2017-10-18 00:00:00")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

for (i in 3:10) {
  cat[[i]] = tsclean(cat[[i]])
}

# filter out seasonality
ts_cat = data.frame(matrix(ncol = 8, nrow = nrow(cat)))
colnames(ts_cat) = colnames(cat[1:8])
cm_cat = list()
for (i in 3:10) {
  ts_cat[i-1] = ts(na.omit(cat[[i]]), frequency=12)
  decomp = stl(ts_cat[[i-1]], s.window="periodic")
  cm_cat[[i-1]] = decomp
  ts_cat[[i-1]] <- seasadj(decomp)
}
names(cm_cat)[2:9] = colnames(cat[,3:10])
for (i in 2:length(cm_cat)) {
  plot(cm_cat[[i]], main=names(cm_cat[i]))[3]
}
