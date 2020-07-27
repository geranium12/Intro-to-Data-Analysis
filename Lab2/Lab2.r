dat<-read.table(file="Data.txt", dec=".")
a<-dat[,1]

# average value
mean(a)
#dispersion
var(a)
#average kv otklonenie
sd(v)
#mediana
median(a)

library(moments)
#assimetria
skewness(a)
#ekscess
kurtosis(a)

#usechennoe srednee
mean(a,trim = 0.05)

#coeff of variation
sd(a)/mean(a)*100

#relative linear deviation
sum(abs(a)-mean(a))/(length(a)*mean(a))

