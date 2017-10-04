#este script es para estar seguros de que en españa los tweets son en español
#y de que en europa no son ni en español ni en catalán. En cat lo dejé abierto.


cat <- read.csv2("catalunya1-0.csv", sep=";", encoding="latin1")
eu <- read.csv2("europe1-0.csv", sep=";", encoding="latin1")
es <- read.csv2("spain1-0.csv", sep=";", encoding="latin1")

require(devtools)
devtools::install_version("cldr",version="1.1.0")
library(cldr)
#demo(cldr)
languageEs <- detectLanguage(es$text)
rowsEs <- which(languageEs$detectedLanguage == "SPANISH" & languageEs$isReliable==TRUE)

languageEu <- detectLanguage(eu$text)
rowsEu <- which(languageEu$detectedLanguage != "CATALAN"|
                  languageEu$detectedLanguage != "SPANISH")


es <- es[rowsEs,]
eu <- eu[rowsEu, ]
