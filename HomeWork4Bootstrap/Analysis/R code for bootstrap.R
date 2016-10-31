x <- c(23.3,26.1,19.0,28.8,29.0)

bootsample1 <- sample(x, size=length(x), replace=TRUE)
bootsample2 <- sample(x, size=length(x), replace=TRUE)
bootsample3 <- sample(x, size=length(x), replace=TRUE)

#What does this do?

R <- 1000
bootmean <- numeric(R)
x <- c(23.3,26.1,19.0,28.8,29.0)
mean(x) # mean of the original sample
for (i in 1:R) {
  bootsample <- sample(x, size=length(x), replace=TRUE)
  bootmean[i] <- mean(bootsample)}

bootmean
summary(bootmean)
sd(bootmean)
hist(bootmean)
####################################
#### Bootstrap Standard Errors #####
## Code to obtain bootstrap SE: ####
####################################
bootsample
  
  x <- c(23.3,26.1,19.0,28.8,29.0)
se.trad <- sd(x)/sqrt(length(x))
for (i in 1:R) {
  bootsample <- sample(x, size=length(x), replace=TRUE)
  bootmean[ i] <- mean(bootsample)}
sd(bootmean)

####################################
##### This puts more than one   ####
#####  plot on the same page    ####
####################################

help("par")  ## <<<<<<<<
par(mfrow = c(1,3))
hist(bootmean)
boxplot(bootmean)
qqnorm(bootmean)
qqline(bootmean)

