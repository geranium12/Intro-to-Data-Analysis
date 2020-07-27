library(MASS)

# 1 
n <- 20
name <- "Herasimchyk"
x1 <- rnorm(n, mean = length(name), sd = length(name) / 3)
y1 <- rnorm(n, mean = length(name), sd = length(name) / 3)

n <- 10
a = 5
x2 <- runif(n, -a, a)
y2 <- runif(n, -a, a)
dat <- cbind(c(x1, x2), c(y1, y2))

n <- 30
n.train <- floor(n * 0.7)
n.test <- n - n.train

# 2

idx.train <- sample(1:n, n.train)
data.train <- dat[idx.train,]
idx.test <- (1:n)[!(1:n %in% idx.train)]
data.test <- dat[idx.test,]

cl <- kmeans(dat, 2)
cl.cluster <- cl$cluster
cl.train <- cl.cluster[idx.train]
cl.test <- cl.cluster[idx.test]

plot(data.train, col=ifelse(cl.train == 1, "blue", "green"))
legend("topleft", legend = c("1", "2"), fill = c("blue", "green"))

mod <- qda(data.train, cl.train)
cl.test_est <- predict(mod, data.test)$class

sum(cl.test_est != cl.test) / n.test             
idx <- idx.test[cl.test_est != cl.test]   

plot(dat,col=ifelse(cl.cluster==1,"blue","green"))
legend("topleft",legend=c("1","2"),fill=c("blue","green"))
points(dat[idx,],col="red")
points(dat[idx.train,],pch=2)

# 3

idx.new<-sample(1:n.train, n.train * 0.2)
for(i in idx.new) 
  cl.train[i] = ifelse(cl.train[i] == 1, 2, 1) 

mod <- qda(data.train, cl.train)
cl.test_est <- predict(mod, data.test)$class

sum(cl.test_est!=cl.test)/n.test             
idx2<-idx.test[cl.test_est!=cl.test]   

plot(dat,col=ifelse(cl.cluster==1,"blue","green"))
legend("topleft",legend=c("1","2"),fill=c("blue","green"))
points(dat[idx2,],col="red")
points(dat[idx.train[idx.new],],pch=3)

