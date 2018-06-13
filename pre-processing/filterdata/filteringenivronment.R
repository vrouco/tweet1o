
#first clean working environment and then load filterdf function. if entering data from es or eu, use filternocatdf function

setwd("~/Dropbox/csv")

files <- list.files()

  fileshere <- grep(paste(substitute(catAll)), files, value=T) #donde catAll se puede poner esAll o euAll y lo debería cargar
  for(i in 1:length(fileshere)){
    assign(sub(".csv","",fileshere[i]),filterdfcat(fileshere[i]))
  }
  
  dfs <- sapply(.GlobalEnv, is.data.frame) 
  mydata <- as.data.frame(do.call(rbind, mget(names(dfs)[dfs])))
  
  mydata$text <- as.character(mydata$text)
  dtparts <- t(as.data.frame(strsplit(as.character(mydata$created),' ')))
  row.names(dtparts) = NULL
  require(chron)
  mydata$created <- chron(dates=dtparts[,1],times=dtparts[,2],format=c('y-m-d','h:m:s'))
  
  aa <- mydata[order(mydata$text, -abs(mydata$retweetCount) ), ] 
  mydata <- aa[ !duplicated(aa$text), ]

write.csv2(mydata, "catalunya1-0.csv", fileEncoding = "latin1")

