# import libraries
lapply(c('dplyr', 'ggplot2', 'lubridate', 'network', 'sna', 'qdap', 'tm', 'XML'),
       library, character.only = TRUE)

# set working directory
setwd("~/Desktop/make hinojosa dictionaries")

############
# HINOJOSA #
############

hino <- read.csv("Hinojosa et al_Supplementary materials.csv", header=TRUE)

# SPANISH #

# arousal
aro.esp <- data.frame(hino$Word_spanish, hino$Ar_Mn)
colnames(aro.esp) <- c("word","arousal")
aro.esp$word <- as.character(aro.esp$word)
aro.esp$arousal <- aro.esp$arousal - 4.5
aro.esp <- distinct(aro.esp)

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

# arousal
aro.cat <- data.frame(hino$Word_catalan, hino$Ar_Mn)
colnames(aro.cat) <- c("word","arousal")
aro.cat$word <- as.character(aro.cat$word)
aro.cat$arousal <- aro.cat$arousal - 4.5
aro.cat <- distinct(aro.cat)

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

# arousal
aro.eng <- data.frame(hino$Word_english, hino$Ar_Mn)
colnames(aro.eng) <- c("word","arousal")
aro.eng$word <- as.character(aro.eng$word)
aro.eng$arousal <- aro.eng$arousal - 4.5
aro.eng <- distinct(aro.eng)

# Amplification
amp.eng <- amplification.words

# Deamplification
dea.eng <- deamplification.words

# Negation
not.eng <- negation.words

# TRILINGUAL LEXICON #
arousal <- as.data.frame(rbind(aro.esp, aro.cat, aro.eng))
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
      polarity(polarity.frame=arousal, negators=negator, amplifiers=amplifier, 
               deamplifiers=deamplifier, constrain=TRUE)
  })
or$arousal = sapply(pol, function(x) x$all$polarity)
or <- subset(or, or$arousal != 0)

# emotional valence time series
filter(or, mday(created) == 1) %>%
  ggplot(aes(created, arousal)) + geom_point() + geom_smooth(span = .5)

# emotional valence on retweets
ggplot(or, aes(x = arousal, y = as.numeric(retweetCount))) +
  geom_point(position = 'jitter') + geom_smooth()
