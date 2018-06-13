library(dplyr)
library(qdap)

setwd("/home/floriane/Downloads/1-Opruebas/1-Opruebas/make short dictionaries")

# SPANISH #
dic.esp <- read.csv2("short-senticon-esp.csv", encoding="latin1") # senticon
amp.esp <- as.character(read.csv2("amplifier-esp.csv", encoding="latin1")$x) # amplfiers
dea.esp <- as.character(read.csv2("deamplifier-esp.csv", encoding="latin1")$x) # deamplfiers
neg.esp <- as.character(read.csv2("negation-esp.csv", encoding="latin1")$x) # negators

# CATALAN #
dic.cat <- read.csv2("short-senticon-cat.csv", encoding="latin1") # senticon
amp.cat <- as.character(read.csv2("amplifier-cat.csv", encoding="latin1")$x) # amplfiers
dea.cat <- as.character(read.csv2("deamplifier-cat.csv", encoding="latin1")$x) # deamplfiers
neg.cat <- as.character(read.csv2("negation-cat.csv", encoding="latin1")$x) # negators

# SPANISH #
dic.eng <- read.csv2("short-senticon-eng.csv") # senticon
amp.eng <- as.character(read.csv2("amplifier-eng.csv")$x) # amplfiers
dea.eng <- as.character(read.csv2("deamplifier-eng.csv")$x) # deamplfiers
neg.eng <- as.character(read.csv2("negation-eng.csv")$x) # negators

tweets <- read.csv2("catalunya1-0.csv", encoding="latin1", sep=";")

# Split into retweets and original tweets

sp = split(tweets, tweets$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

Sys.setlocale(locale="C")

#`[[.qdap_hash` <- `[[.data.frame`
pol = 
  lapply(orig$text, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity(polarity.frame=dic.esp, negators=not.esp, amplifiers=amp.esp, 
               deamplifiers=dea.esp)
  })
orig$emotionalValence = sapply(pol, function(x) x$all$polarity)

# As reality check, what are the most and least positive tweets
orig$text[which.max(orig$emotionalValence)]
orig$text[which.min(orig$emotionalValence)]
# How does emotionalValence change over the day?
filter(orig, mday(created) == 22) %>%
  ggplot(aes(created, emotionalValence)) +
  geom_point() + 
  geom_smooth(span = .5)