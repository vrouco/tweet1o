
#csve <- "catAll 2017-10-01 15.47.01.csv"



filterdfcat <- function(csv){
  
  call <- read.csv(csv,encoding = "latin1", sep=";")
  call$or.text <- call$text
  call$text <- as.character(call$text)
  Encoding(call$text)<-"latin1"
  call$text <- gsub("\n"," ", call$text)
  splitText <- strsplit(call$text, split=" ")
  
  #urls
  for (i in 1:length(splitText)){    
    call$url[i] <- as.character(list(grep("http",splitText[[i]], value=T)))
  }
  call$url[which(call$url=="character(0)")] <- NA
  call$text <- gsub("http\\w+ *", "", call$text) #remove http:...
  call$text <- gsub("://t.co/[a-zA-Z0-9_]+ *", "", call$text)
  
  # at
  for (i in 1:length(splitText)){    
    call$at[i] <- as.character(list(grep("@",splitText[[i]], value=T)))
  }
  call$at[which(call$at=="character(0)")] <- NA
  call$at <- gsub(":", "", call$at)
  call$text <- gsub("@\\w+ *", "", call$text)
  
  #hashtags
  for (i in 1:length(splitText)){    
    call$hashtag[i] <- as.character(list(grep("#",splitText[[i]], value=T)))
  }
  call$hashtag[which(call$hashtag=="character(0)")] <- NA
  call$hashtag <- gsub(":", "", call$hashtag)
  call$text <- gsub("#\\w+ *", "", call$text)
  
  #RTs
  for (i in 1:length(splitText)){    
    call$is.RT[i] <- as.character(list(grep("RT",splitText[[i]], value=T)))
  }
  call$is.RT <- ifelse(call$is.RT=="RT", TRUE, FALSE)
  call$text <- gsub("RT : ", "", call$text)
  call$text <- gsub("RT", "", call$text)
  
  require(qdap)
  call$text <- qprep(call$text)
                    
  call <- call[!duplicated(call$text),]
  return(call)
}




filterdfnocat <- function(csv){   #la differenia es q hay que cortar la bdd para que no coja tweets en cataluña
  
  call <- read.csv(csv,encoding = "latin1", sep=";")
  call$or.text <- call$text
  call$text <- as.character(call$text)
  Encoding(call$text)<-"latin1"
  call$text <- gsub("\n"," ", call$text)
  splitText <- strsplit(call$text, split=" ")
  
  #urls
  for (i in 1:length(splitText)){    
    call$url[i] <- as.character(list(grep("http",splitText[[i]], value=T)))
  }
  call$url[which(call$url=="character(0)")] <- NA
  call$text <- gsub("http\\w+ *", "", call$text) #remove http:...
  call$text <- gsub("://t.co/[a-zA-Z0-9_]+ *", "", call$text)
  
  # at
  for (i in 1:length(splitText)){    
    call$at[i] <- as.character(list(grep("@",splitText[[i]], value=T)))
  }
  call$at[which(call$at=="character(0)")] <- NA
  call$at <- gsub(":", "", call$at)
  call$text <- gsub("@\\w+ *", "", call$text)
  
  #hashtags
  for (i in 1:length(splitText)){    
    call$hashtag[i] <- as.character(list(grep("#",splitText[[i]], value=T)))
  }
  call$hashtag[which(call$hashtag=="character(0)")] <- NA
  call$hashtag <- gsub(":", "", call$hashtag)
  call$text <- gsub("#\\w+ *", "", call$text)
  
  #RTs
  for (i in 1:length(splitText)){    
    call$is.RT[i] <- as.character(list(grep("RT",splitText[[i]], value=T)))
  }
  call$is.RT <- ifelse(call$is.RT=="RT", TRUE, FALSE)
  call$text <- gsub("RT : ", "", call$text)
  call$text <- gsub("RT", "", call$text)
  
  call <- call[1:800, ]   #este es el trimming, único para nocat
  
  require(qdap)
  call$text <- qprep(call$text)
  
  call <- call[!duplicated(call$text),]
  return(call)
}

#data <- filterdf(csv)
