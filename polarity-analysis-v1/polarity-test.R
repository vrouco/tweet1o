# import libraries
lapply(c('dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm', 'XML'),
       library, character.only = TRUE)

# set working directory
setwd("/home/alberto/Desktop/make short dictionaries")

#############
# SENTICONS #
#############

# SPANISH #

# Positive
xm <- xmlParse("senticon.es.pos.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
pos.esp <- data.frame(ma)
colnames(pos.esp) <- c("word","pos", "pol", "std")
pos.esp$word <- as.character(gsub(" ", "", pos.esp$word))
pos.esp$pol <- as.numeric(as.character(pos.esp$pol))
pos.esp$std <- as.numeric(as.character(pos.esp$std))

# Negative
xm <- xmlParse("senticon.es.neg.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
neg.esp <- data.frame(ma)
colnames(neg.esp) <- c("word","pos", "pol", "std")
neg.esp$word <- gsub(" ", "", neg.esp$word)
neg.esp$pol <- as.numeric(as.character(neg.esp$pol))
neg.esp$std <- as.numeric(as.character(neg.esp$std))

# Amplification
amp.esp <- as.character(c("agudo", "aguda", "agudamente", "cierto", "cierta",
                          "ciertamente", "colosal", "colosalmente", "profundo",
                          "profunda", "profundamente", "definitivo", "definitiva",
                          "definitivamente", "enorme", "enormemente", "extremo",
                          "extrema", "extremado", "extremada", "extremadamente",
                          "gran", "grande", "grandemente", "fuertemente", 
                          "fuerte", "alto", "alta", "altamente", "enorme", 
                          "enormemente", "inmenso", "inmensa", "inmensamente",
                          "incalculable", "incalculablemente", "masivo", "masiva",
                          "masivamente", "más", "particular", "particularmente",
                          "a propósito", "deliberadamente", "muy", "real", 
                          "realmente", "grave", "gravemente", "severo", "severa",
                          "severamente", "significativo", "significativa", 
                          "significativamente", "seguro", "segura", "seguramente",
                          "verdadero", "verdadera", "verdaderamente", "vasto", 
                          "vasta", "vastamente", "mucho", "mucha"))

# Deamplification
dea.esp <- as.character(c("apenas", "escasamente", "poco", "difícilmente", 
                          "escaso", "escasa", "solo", "raramente", "ligeramente", 
                          "poco", "poca", "esporádico", "esporádica", 
                          "esporádicamente", "muy_poco", "menos", "solamente"))

# Negation
not.esp <- as.character(c("ni", "nunca", "no", "nadie", "nada"))

# CATALAN #

# Positive
xm <- xmlParse("senticon.ca.pos.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
pos.cat <- data.frame(ma)
colnames(pos.cat) <- c("word","pos", "pol", "std")
pos.cat$word <- gsub(" ", "", pos.cat$word)
pos.cat$pol <- as.numeric(as.character(pos.cat$pol))
pos.cat$std <- as.numeric(as.character(pos.cat$std))

# Negative
xm <- xmlParse("senticon.ca.neg.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
neg.cat <- data.frame(ma)
colnames(neg.cat) <- c("word","pos", "pol", "std")
neg.cat$word <- gsub(" ", "", neg.cat$word)
neg.cat$pol <- as.numeric(as.character(neg.cat$pol))
neg.cat$std <- as.numeric(as.character(neg.cat$std))

# Amplification
amp.cat <- as.character(c("agut", "aguda", "agudament", "cert", "certa",
                          "certament", "colossal", "colossalment", "profund",
                          "profunda", "profundament", "definitiu", "definitiva",
                          "definitivament", "enorme", "enormement", "extrem",
                          "extrema", "extremat", "extremada", "extremadament",
                          "gran", "granment", "fortament", "forta",
                          "fort", "alt", "alta", "altament", "enorme",
                          "enormement", "immens", "immensa", "immensament",
                          "incalculable", "incalculablement", "massiu", "massiva",
                          "Massivament", "més", "particular", "particularment",
                          "a propósit", "deliberadament", "molt", "real",
                          "realment", "greu", "greument", "sever", "severa",
                          "severament", "significatiu", "significativa",
                          "significativament", "segur", "segura", "segurament", 
                          "de segur", "veritable", "veritablement", "vast",
                          "vasta", "vastament", "molt", "molta"))

# Deamplification
dea.cat <- as.character(c("tot_just", "escassament", "poc", "difícilment", 
                          "escàs", "escassa", "rarament", "lleugerament", "poc", 
                          "poca", "esporàdic", "esporàdica", "esporàdicament", 
                          "molt_poc", "menys", "només"))

# Negation
not.cat <- as.character(c("ni", "mai", "no", "ningú", "cap", "pas", "gens"))

# ENGLISH #

# Positive
xm <- xmlParse("senticon.en.pos.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
pos.eng <- data.frame(ma)
colnames(pos.eng) <- c("word","pos", "pol", "std")
pos.eng$word <- gsub(" ", "", pos.eng$word)
pos.eng$pol <- as.numeric(as.character(pos.eng$pol))
pos.eng$std <- as.numeric(as.character(pos.eng$std))

# Negative
xm <- xmlParse("senticon.en.neg.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
neg.eng <- data.frame(ma)
colnames(neg.eng) <- c("word","pos", "pol", "std")
neg.eng$word <- gsub(" ", "", neg.eng$word)
neg.eng$pol <- as.numeric(as.character(neg.eng$pol))
neg.eng$std <- as.numeric(as.character(neg.eng$std))

# Amplification
amp.eng <- amplification.words

# Deamplification
dea.eng <- deamplification.words

# Negation
not.eng <- negation.words

# TRILINGUAL SENTICON #
esp.sent <- sentiment_frame(pos.esp$word, neg.esp$word)
cat.sent <- sentiment_frame(pos.cat$word, neg.cat$word)
eng.sent <- sentiment_frame(pos.eng$word, neg.eng$word)
senticon <- as.data.frame(rbind(esp.sent, cat.sent, eng.sent))

amplifier <- c(amp.esp, amp.cat, amp.eng)
deamplifier <- c(dea.esp, dea.cat, dea.eng)
negator <- c(not.esp, not.cat, not.eng)

#####################
# POLARITY ANALYSIS #
#####################

# data
tweets <- read.csv2("catalunya1-0.csv", encoding="latin1", sep=";")

# Split into retweets and original tweets
sp = split(tweets, tweets$isRetweet)
orig = sp[['FALSE']]

# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

# drop little-retweeted observations
orig$retweetCount <- as.numeric(orig$retweetCount)
orig <- subset(orig, orig$retweetCount > 10)

# drop drunk observations (super-slow, sorry)
or <- data.frame()
for (i in 1:1000) {
  beg <- substr(orig$created[i], 1, 1)
  if (beg == "(" && !is.na(beg)) {
    or <- rbind(or[,], orig[i,])
  }
}

# set time properly
or$created <- gsub("\\(", "", or$created)
or$created <- gsub("\\)", "", or$created)
or$created <- as.POSIXct(or$created, tz="Spain/Madrid")

# apply polarity analysis (slow too)
pol = 
  lapply(or$text, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity(polarity.frame=senticon, negators=negator, amplifiers=amplifier, 
               deamplifiers=deamplifier)
  })
or$emotionalValence = sapply(pol, function(x) x$all$polarity)

# emotional valence time series
filter(or, mday(created) == 1) %>%
  ggplot(aes(created, emotionalValence)) + geom_point() + geom_smooth(span = .5)

# emotional valence on retweets
ggplot(or, aes(x = emotionalValence, y = as.numeric(retweetCount))) +
  geom_point(position = 'jitter') + geom_smooth()
