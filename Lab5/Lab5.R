dat<-read.table(file="Data.txt", dec=".")
v<-dat

v[, 1] <- scale(v[, 1])
v[, 2] <- scale(v[, 2])

# 2 clusters

cl <- kmeans(v, 2)
table(cl$cluster)

cl$centers

plot(v, col=ifelse(cl$cluster == 1, "blue", "red"))
legend("topleft", legend = c("1", "2"), fill = c("blue", "red"))

plot(v, pch = ifelse(cl$cluster == 1, 1, 2))
legend("topleft", legend = c("1", "2"), pch = c(1, 2))

# 3 clusters

cl <- kmeans(v, 3)
table(cl$cluster)

cl$centers

plot(v, col=ifelse(cl$cluster == 1, "blue",
                   ifelse(cl$cluster == 2, "red", "green")))
legend("topleft", legend = c("1", "2", "3"), fill = c("blue", "red", "green"))

plot(v, pch = ifelse(cl$cluster == 1, 1, 
                     ifelse(cl$cluster == 2, 2, 3)))
legend("topleft", legend = c("1", "2", "3"), pch = c(1, 2, 3))

