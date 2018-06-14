library(here)
library(tidyverse)

setwd(here("data/participants/lexicon to data/")) 


es.or <- read.csv("data$lexicon es.csv")
eu <- read.csv("data$lexicon eu.csv")
cat <- read.csv("data$lexicon cat.csv")


es <- summary.data(es.or)
