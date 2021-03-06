

setwd("C:/Users/Angel Mail 1/Desktop/albert/SMU data science/Course Work/Doing Data Science/A6CollaborationAndVersioningBootStrap/HomeWork4Bootstrap/Analysis")

library(repmis)

R <= 5000
##This creates the first normal distribution
## N = 20  mean = 100  sd = 1
Normal20<- rnorm(20,100,1)

Bootnorm20 <- numeric(R)
for (i in 1:R) {
  Bootsample <- sample(Normal20, size=length(x), replace=TRUE)
  Bootnorm20[i] <- mean(Bootsample)}

###### sample 2 n 50 normal
##This creates the first normal distribution
## N = 50  mean = 100  sd = 1
Normal50 <- rnorm(50,100,1)

bootnorm50 <- numeric(R)
for (i in 1:R) {
  bootsample <- sample(Normal50, size=length(x), replace=TRUE)
  bootnorm50[i] <- mean(bootsample)}


#**** first exp distribution n = 20
Exp20 <- rexp(20)
Bootexp20 <- numeric(R)
for (i in 1:R) {
  Bootsample <- sample(Exp20, size=length(x), replace=TRUE)
  Bootexp20[i] <- mean(Bootsample)}

#**** second exp distribution n = 50
Exp50 <- rexp(50)
Bootexp50 <- numeric(R)
for (i in 1:R) {
  bootsample <- sample(Exp50, size=length(x), replace=TRUE)
  Bootexp50[i] <- mean(bootsample)}


##Writing Datasets
setwd("Data")

write.csv(Normal20,"Normal20")
write.csv(Bootnorm20,"Bootnorm20")
write.csv(Normal50,"Normal50")
write.csv(Bootnorm50,"Bootnorm50")
write.csv(Exp20,"Exp20")
write.csv(Bootexp20,"Bootexp20")
write.csv(Exp50,"Exp50")
write.csv(Bootexp50,"Bootexp50")
getwd()

setwd("..")


#### Summaries ##########

## Preliminary Data Analysis
str(Normal20)
str(Normal50)
str(Exp20)
str(Exp50)
str(Bootnorm20)
str(Bootnorm50)
str(Bootexp20)
str(Bootexp50)

##All vectors created as requested

##Comparing Normal20 to Normal50

summary(Normal20,digits = 3)
summary(Normal50,digits = 3)
sd(Normal20)
sd(Normal50)

par(mfrow = c(3,2))

hist(Normal20,main = "Histogram of\n Normal Distribution\n n=20", sub = "Mean = 99.9\n sd =1.376",xlab='', col=5)
hist(Normal50,main = "Histogram of\n Normal Distribution\n n=50", sub = "Mean = 99.9\n sd =0.876",xlab='',col=6)
boxplot(Normal20,main="Boxplot of\n Normal Distribution\n n=20" ,horizontal=TRUE, xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 97.5    99.1    99.5    99.9   101.0   103.0  1.376"  ,col=5, notch = TRUE)
boxplot(Normal50,main="Boxplot of\n Normal Distribution\n n=5000",horizontal=TRUE,xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 98.2    99.4    99.8    99.9   101.0   102.0  0.876" ,col=6, notch = TRUE)
qqnorm(Normal20,col=5)
qqline(Normal20)
qqnorm(Normal50,col=6)
qqline(Normal50)


# Comparing Normal20 Bootstrap 20
par(mfrow = c(3,2))

summary(Normal20,digits = 3)
summary(Bootnorm20,digits = 3)
sd(Normal20)
sd(Bootnorm20)

par(mfrow = c(3,2))

hist(Normal20,main = "Histogram of\n Normal Distribution\n n=20",                           sub = "Mean = 99.9\n sd =1.376",xlab='', col=5)
hist(Bootnorm20,main = "Histogram of Bootstrap\n Normal Distribution\n n=20 Samples=5,000", sub = "Mean = 99.8\n sd =1.353",xlab='',col=6)
boxplot(Normal20,main="Boxplot of\n Normal Distribution\n n=20" ,horizontal=TRUE,                        xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 97.5    99.1    99.5    99.9   101.0   103.0  1.376"  ,col=5, notch = TRUE)
boxplot(Bootnorm20,main="Boxplot of Bootstrap\n Normal Distribution",horizontal=TRUE,sub="5,000 Samples",xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 97.5    99.0    99.4    99.8   101.0   103.0  1.353" ,col=6, notch = TRUE)
qqnorm(Normal20,col=5)
qqline(Normal20)
qqnorm(Bootnorm20,col=6)
qqline(Bootnorm20)


# Comparing Normal50 Bootstrap 50
par(mfrow = c(3,2))

summary(Normal50,digits = 3)
summary(Bootnorm50,digits = 3)
sd(Normal50)
sd(Bootnorm50)

par(mfrow = c(3,2))

hist(Normal50,main = "Histogram of\n Normal Distribution\n n=50", sub = "Mean = 100.0\n sd =1.046",xlab='', col=5)
hist(Bootnorm50,main = "Histogram of Bootstrap\n Normal Distribution\n n=50 Samples=5,000", sub = "Mean = 99.9\n sd =1.225",xlab='',col=6)
boxplot(Normal50,main="Boxplot of\n Normal Distribution\n n=50" ,horizontal=TRUE,                        xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 98.2    99.4    99.8    99.9   101.0   102.0   0.876"  ,col=5, notch = TRUE)
boxplot(Bootnorm50,main="Boxplot of Bootstrap\n Normal Distribution",horizontal=TRUE,sub="5,000 Samples",xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 97.4    99.0    99.8    99.9   101.0   103.0   1.225" ,col=6, notch = TRUE)
qqnorm(Normal50,col=5)
qqline(Normal50)
qqnorm(Bootnorm50,col=6)
qqline(Bootnorm50)


# Comparing Exp20 BootstrapExpontential20
par(mfrow = c(3,2))

summary(Exp20,digits = 3)
summary(Bootexp20,digits = 3)
sd(Exp20)
sd(Bootexp20)

par(mfrow = c(3,2))

hist(Exp20,main = "Histogram of\n Exponential Distribution\n n=50", sub = "Mean = 1.19\n sd =1.76",xlab='', col=5)
hist(Bootexp20,main = "Histogram of Bootstrap\n Exponential Distribution\n n=50 Samples=5,000", sub = "Mean = 1.18\n sd =1.694",xlab='',col=6)
boxplot(Exp20,main="Boxplot of\n Exponential Distribution\n n=50" ,horizontal=TRUE,                          xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 0.00097 0.27300 0.63200 1.19000 0.95500 7.48000   1.765"  ,col=5, notch = TRUE)
boxplot(Bootexp20,main="Boxplot of Bootstrap\n Exponential Distribution",horizontal=TRUE,sub="5,000 Samples",xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 0.00097 0.27700 0.64300 1.18000 0.92100 7.48000   1.694" ,col=6, notch = TRUE)
qqnorm(Exp20,col=5)
qqline(Exp20)
qqnorm(Bootexp20,col=6)
qqline(Bootexp20)



# Comparing Exp50 BootstrapExpontential50
par(mfrow = c(3,2))

summary(Exp50,digits = 3)
summary(Bootexp50,digits = 3)
sd(Exp50)
sd(Bootexp50)

par(mfrow = c(3,2))

hist(Exp50,main = "Histogram of\n Exponential Distribution\n n=50", sub = "Mean = 0.897\n sd =0.733",xlab='', col=5)
hist(Bootexp50,main = "Histogram of Bootstrap\n Exponential Distribution\n n=50 Samples=5,000", sub = "Mean = 0.90\n sd =.729",xlab='',col=6)
boxplot(Exp50,main="Boxplot of\n Exponential Distribution\n n=50" ,horizontal=TRUE,                          xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 0.016   0.248   0.826   0.897   1.340   2.580   0.733"  ,col=5, notch = TRUE)
boxplot(Bootexp50,main="Boxplot of Bootstrap\n Exponential Distribution",horizontal=TRUE,sub="5,000 Samples",xlab="Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n 0.016   0.245   0.803   0.900   1.390   2.580   0.729" ,col=6, notch = TRUE)
qqnorm(Exp50,col=5)
qqline(Exp50)
qqnorm(Bootexp50,col=6)
qqline(Bootexp50)



