setwd("~/Documents/GitHub/tweet1o")

require(gdata)
df <- read.xls ("EMOTE.xlsx", sheet = 1, header = TRUE)
words <- paste(as.character(df$word), collapse=".\n ")
words <- strsplit(words, split=" ")
words <- unlist(words)
write.table(words[1:500], "originalwordsEMOTE.txt", row.names = F)

require(translate)

getwd()
read.table("EMOTEcat.txt")
"file:///C:/The R-help January 2009 Archive by date.html"