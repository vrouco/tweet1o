

processing <- function(sample, language){
  #INPUT FUNC
  library(tidyverse)
  mydata <- read_lex(sample)
  #PICK BASED ON LANGUAGE
  #mydata <- pick(language)
  return(mydata)
  }


#INPUT FUNCT
read_lex <- function(sample){
  library("here")
  setwd(here::here("/data/participants"))
  if(as.character(sample) =="es"){
    lexicon <- read.csv("es_db_indian.csv", sep=",", stringsAsFactors = F)
  }else{NA}
  return(lexicon)
}

#PICK BASED ON LANGUAGE
pick <- function(language){
  require(cld2)
  this.language <- detect_language(mydata$cleaned_text)
rowstokeep <- which(this.language == language)
mydata <- mydata[which(this.language == language),]
return(mydata)
}


eu$created <- strptime(eu$created,format= "%Y-%m-%d %H:%M:%S")
  