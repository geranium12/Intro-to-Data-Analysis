dat<-read.table(file="Data.txt", dec=".")
dat
x<-dat[, 1]
y<-dat[, 2]

#correlation field
plot(dat,type="p",main="Correlation field",xlab="X", ylab="Y")



#create an empty table
t <- rep(0, 3*5)
dim(t) <- c(3, 5)
t1 <- as.data.frame(t)


x_mean <- mean(x)
X_sd <- sd(x)
t1[1:3, 1]<-x_mean - (1:3) * X_sd
t1[1:3, 2]<-x_mean + (1:3) * X_sd

for(i in 1:3) {
  t1[i, 3]<-length(which(x >= t1[i, 1] & x < t1[i, 2]))
}

t1[1:3, 4]<-t1[1:3, 3] / length(x) * 100
t1[1:3, 5]<-c(68.3, 95.4, 99.7)
t1

n<-1+floor(log2(length(x)))
t<-rep(0, n*5)
dim(t)<-c(n, 5)
t2<-as.data.frame(t)
interval_length<-(max(x) - min(x)) / n
t2[1:n, 1]<- min(x) + (1:n - 1) * interval_length
t2[1:n, 2]<-min(x) + (1:n) * interval_length
for (i in 1:n) {
  if (i != n) {
    t2[i, 3]<-length(which(x >= t2[i, 1] & x < t2[i, 2]))
  }
  else {
    t2[i, 3]<-length(which(x >= t2[i, 1] & x <= t2[i, 2]))
  }
}
for (i in 1:n) {
  t2[i, 4]<-0
  for (j in 1:length(y)) {
    if (x[j] < t2[i, 1]) next
    if (x[j] > t2[i, 2]) next
    if (x[j] == t2[i, 2] & i != n) next
    t2[i, 4] = t2[i, 4] + y[j]
  }
}

t2[1:n, 5]<-t2[1:n, 4] / t2[1:n, 3]
t2

cor(x, y)
#t
abs(cor(x, y)) * sqrt((length(x) - 2 / 1 - cor(x, y)^2))
plot(dat, type="p", xlab="X", ylab="Y")
#regression equation
lm(y ~ x)
#add line
abline(lm(y ~ x))

t1
t2
