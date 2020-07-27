#1
dat<-read.table(file="Data.txt", dec=".")
x<-dat[, 1]

#2
#формула средней хронологической моментного ряда
length_x <- length(x)
chrono_moment_series <- sum(x)
chrono_moment_series <- chrono_moment_series - (x[1] / 2)
chrono_moment_series <- chrono_moment_series - (x[length_x] / 2)
chrono_moment_series <- chrono_moment_series / (length_x - 1)

#3 наличие тренда
series_mean <- mean(x)
series_table <- rep(0, length_x*4)
dim(series_table) <- c(length_x, 4)
series_table <- as.data.frame(x)
series_table[1:length_x, 2] <- ifelse(x[1:length_x] < series_mean, 1, 0)
number_of_series<-length(rle(series_table[,2])$lengths)

# при p = 0.95
t = 1.96
# критерий серий
average_number_of_series <- length_x + 1 / 2
number_of_series_sqrt <- sqrt(length_x + 1) / 2
if (number_of_series <= (average_number_of_series + t * number_of_series_sqrt) &
    number_of_series >= (average_number_of_series - t * number_of_series_sqrt)) {
  print("YES")
  } else {
    print("NO")
  }

#4 сглаживание ряда динамики методом трехуровневой прямой
#smoothing_vector <- rep(0, length_x)
#smoothing_vector[1] <- x[1]
for (i in 2:(length_x - 1))
{
  series_table[i, 3] <- ((x[i - 1] + x[i] + x[i + 1]) / 3)
}
series_table[1, 3] <- (5 * x[1] + 2 * x[2] - x[3]) / 6
series_table[length_x, 3] <- (5 * x[length_x] + 2 * x[length_x - 1] - x[length_x - 2]) / 6

# 5-6
a0 <- chrono_moment_series
p = 0
for (i in 1:length_x) 
{
  series_table[i, 4] <- -floor(length_x / 2) + i - 1
}
a1 <- sum(series_table[,2] * series_table[, 4])
a1 <- a1 / sum(series_table[, 4] ^ 2)
series_table[1:length_x, 5]<-a0 + a1 * series_table[1:length_x, 1]
series_table

# 7
plot(series_table[, 1],type="l",main="", col = "purple", xlab="X", ylab="Y")
lines(series_table[, 3], type = "l", col="green")
lines(series_table[, 5], type = "l", col="red")


