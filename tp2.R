#Exercice 1 : Importer les données
library(readxl)
dataset <- read_excel(path  = "C:/Users/qhua/Downloads/pokemon.xlsx", sheet = "pokemon")
dim(dataset)
names(dataset)
str(dataset)
dataset$generation = as.factor(dataset$generation)
dataset$is_legendary = as.factor(dataset$is_legendary)
dataset$type = as.factor(dataset$type)
nlevels(dataset$generation)
summary(dataset)

#Exercice 2 : Statistiques descriptives
mean(dataset$weight_kg,na.rm = T)
median(dataset$weight_kg,na.rm = T)
summary(dataset$height_m)
quantile(dataset$height_m, seq(0,1,0.1),na.rm = T)
var(dataset$weight_kg, na.rm = T)
sd(dataset$weight_kg, na.rm = T)
sort(table(dataset$generation, useNA = "ifany"), decreasing = T)
sort(table(dataset$is_legendary, useNA = "ifany"), decreasing = T)
sort(table(dataset$type, useNA = "ifany"), decreasing = T)

#Exercice 3 : Tris et sélections
a <- dataset[c(2,4)]
dim(a)
b <- dataset[seq(50),seq(2)]
dim(b)
c <- dataset[seq(10),seq(ncol(dataset))]
dim(c)
d <- dataset[seq(ncol(dataset)-1)]
dim(c)
rang <- order(dataset$nom)
dataset = dataset[rang,]
e = dataset[1,2]
e
rang <- order(dataset$weight_kg, decreasing = T)
dataset = dataset[rang,]
f = dataset[1,2]
f
rang <- order(- dataset$attack, dataset$speed)
dataset = dataset[rang,]
g = dataset[seq(10),2]
g

#Exercice 4 : Tris et filtres
a <- dataset[dataset$attack >= 150, c("nom","attack")]
a = a[order(a$attack, decreasing = T),]
dim(a)
b <- dataset[dataset$type %in% c("dragon", "ghost", "psychic", "dark"), c("nom","type")]
dim(b)
c <- dataset[dataset$type == "fire" & dataset$attack >= 100, c("nom","type","attack")]
c = c[order(c$attack, decreasing),]
dim(c)
d <- dataset[100 <= dataset$speed & dataset$speed <= 150, c("nom","speed")]
d = d[order(d$speed, decreasing = T),]
dim(d)
e <- dataset[is.na(dataset$height_m), c("nom","height_m")]
dim(e)
f <- dataset[!is.na(dataset$height_m)&!is.na(dataset$weight_kg),c("nom","weight_kg","height_m")]
dim(f)
g <- dataset[dataset$weight_kg >= 250, c("nom","weight_kg")]
g <- na.omit(g)
dim(g)
View(g)

#Exercice 5 : Agrégations
