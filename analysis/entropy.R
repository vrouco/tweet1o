cat = read.csv2("cat ts with val-ar from model.csv", sep=";")
esp = read.csv2("es ts with val-ar from model.csv", sep=";")

library(entropy)

ent <- data.frame(cat=as.integer(rep(0,8)),esp=as.integer(rep(0,8)), 
                  vars=as.character(colnames(cat[,2:9])))
for (i in 2:9) {
  ent$cat[i-1] <- entropy(cat[,i])
  ent$esp[i-1] <- entropy(esp[,i])
}
