dat<-read.table(file="Data.txt", dec=".")
a<-table(dat)
f<-as.data.frame(a)

f[,3]<-100 * a / sum(a)
f[,4]<-cumsum(a)
f[,5]<-cumsum(f[,3])
colnames(f)<-c("Value","Frequency1","Frequency2","Accumulated frequency1","Accumulated frequency2")

plot(a, type="l",main="Frequency poligon",xlab="Value",ylab="Frequency")
d<-dat[,1]
v<-sort(d)
x<-unique(v)
y<-as.numeric(f[,4])
plot(x,y,type="l",xlab="Value",ylab="Accumulated Frequency1", main="Cumulative absolute frequencies")
y<-as.numeric(f[,5])
plot(x,y,type="l",xlab="Value",ylab="Accumulated Frequency2", main="Cumulative relative frequencies")