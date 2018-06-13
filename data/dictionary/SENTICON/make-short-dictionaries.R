# import libraries
require(XML)
library(dplyr)

# set working directory
setwd("/home/floriane/Downloads/1-Opruebas/1-Opruebas/make short dictionaries")

#############
# SENTICONS #
#############

# SPANISH #

# Positive
xm <- xmlParse("senticon.es.pos.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
da <- data.frame(ma)
colnames(da) <- c("word","pos", "pol", "std")
pos.esp <- as.character(da$word)

# Negative
xm <- xmlParse("senticon.es.neg.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
da <- data.frame(ma)
colnames(da) <- c("word","pos", "pol", "std")
neg.esp <- as.character(da$word)

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
                          "esporádicamente", "muy poco", "menos", "solamente"))

# Negation
not.esp <- as.character(c("ni", "nunca", "no", "nadie", "nada"))

# make dictionary
dic.esp <- data.frame(cbind(c(pos.esp, neg.esp), c(rep(1, length(pos.esp)), 
                                                   rep(-1, length(neg.esp)))))
colnames(dic.esp) <- c("x", "y")

# CATALAN #

# Positive
xm <- xmlParse("senticon.ca.pos.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
da <- data.frame(ma)
colnames(da) <- c("word","pos", "pol", "std")
pos.cat <- as.character(da$word)

# Negative
xm <- xmlParse("senticon.ca.neg.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
da <- data.frame(ma)
colnames(da) <- c("word","pos", "pol", "std")
neg.cat <- as.character(da$word)

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
dea.cat <- as.character(c("tot just", "escassament", "poc", "difícilment", 
                          "escàs", "Escassa", "rarament", "lleugerament", "poc", 
                          "Poca", "esporàdic", "esporàdica", "esporàdicament", 
                          "Molt poc", "menys", "només"))

# Negation
not.cat <- as.character(c("ni", "mai", "no", "ningú", "cap", "pas", "gens"))

# make dictionary
dic.cat <- data.frame(cbind(c(pos.cat, neg.cat), c(rep(1, length(pos.cat)), 
                                                   rep(-1, length(neg.cat)))))
colnames(dic.cat) <- c("x", "y")

# ENGLISH #

# Positive
xm <- xmlParse("senticon.en.pos.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
da <- data.frame(ma)
colnames(da) <- c("word","pos", "pol", "std")
pos.eng <- as.character(da$word)

# Negative
xm <- xmlParse("senticon.en.neg.xml")
li <- xmlToList(xm)
ar <- array(unlist(li))
ma <- matrix(ar, nrow=length(ar)/4, ncol=4, byrow=TRUE)
da <- data.frame(ma)
colnames(da) <- c("word","pos", "pol", "std")
neg.eng <- as.character(da$word)

# Amplification
amp.eng <- amplification.words

# Deamplification
dea.eng <- deamplification.words

# Negation
not.eng <- negation.words

# make dictionary
dic.eng <- data.frame(cbind(c(pos.eng, neg.eng), c(rep(1, length(pos.eng)), 
                                                   rep(-1, length(neg.eng)))))
colnames(dic.eng) <- c("x", "y")

# WRITE DICTIONARIES #
write.csv2(dic.cat, "short-senticon-cat.csv", fileEncoding="latin1")
write.csv2(amp.cat, "amplifier-cat.csv", fileEncoding="latin1")
write.csv2(dea.cat, "deamplifier-cat.csv", fileEncoding="latin1")
write.csv2(not.cat, "negation-cat.csv", fileEncoding="latin1")
write.csv2(dic.esp, "short-senticon-esp.csv", fileEncoding="latin1")
write.csv2(amp.esp, "amplifier-esp.csv", fileEncoding="latin1")
write.csv2(dea.esp, "deamplifier-esp.csv", fileEncoding="latin1")
write.csv2(not.esp, "negation-esp.csv", fileEncoding="latin1")
write.csv2(dic.eng, "short-senticon-eng.csv", fileEncoding="latin1")
write.csv2(amp.eng, "amplifier-eng.csv", fileEncoding="latin1")
write.csv2(dea.eng, "deamplifier-eng.csv", fileEncoding="latin1")
write.csv2(not.eng, "negation-eng.csv", fileEncoding="latin1")
