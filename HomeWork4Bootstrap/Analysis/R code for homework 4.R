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


#/**** first exp distribution n = 20
Exp20 <- rexp(20)
Bootexp20 <- numeric(R)
for (i in 1:R) {
  Bootsample <- sample(Exp20, size=length(x), replace=TRUE)
  Bootexp20[i] <- mean(Bootsample)}

#/**** second exp distribution n = 50
Exp50 <- rexp(50)
Bootexp50 <- numeric(R)
for (i in 1:R) {
  bootsample <- sample(Exp50, size=length(x), replace=TRUE)
  Bootexp50[i] <- mean(bootsample)}

##Writing Datasets
setwd("Data")

write.csv(Normal20,"Normal20")
write.csv(Normal50,"Normal50")
write.csv(Exp20,"Exp20")
write.csv(Exp50,"Exp50")

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
par(mfrow = c(3,2))

hist(Normal20,col=5)
hist(Normal50,col=6)
boxplot(Normal20,main="Boxplot Normal20" ,xlab="Random Normal Sample",col=5)
boxplot(Normal50,main="Boxplot Normal50",xlab="Random Normal Sample",col=6)
qqnorm(Normal20,col=5)
qqline(Normal20)
qqnorm(Normal50,col=6)
qqline(Normal50)

sumN20 <- summary(Normal20)
sumN50 <- summary(Normal50)
sdN20 <- sd(Normal20)
sdN50 <- sd(Normal50)


#Normal20 to bootnorm20
hist(Normal20,col=5)
hist(Bootnorm20,col=6)
boxplot(Normal20,main="Boxplot Normal20" ,xlab="Random Normal Sample",col=5)
boxplot(Bootnorm20,main="Boxplot Bootnorm20",xlab="5,000 Samples",col=6)
qqnorm(Normal20,col=5)
qqline(Normal20)
qqnorm(bootnorm50,col=6)
qqline(bootnorm50)

summary(Normal20)
summary(Bootnorm20)
sd(Normal20)
sd(Bootnorm20)



#Normal50 to bootnorm50
hist(Normal50,col=5)
hist(Bootnorm50,col=6)
boxplot(Normal50,main="Boxplot Normal50" ,xlab="Random Normal Sample",col=5)
boxplot(Bootnorm50,main="Boxplot Bootnorm50",xlab="5,000 Samples",col=6)
qqnorm(Normal50,col=5)
qqline(Normal50)
qqnorm(bootnorm50,col=6)
qqline(bootnorm50)

summary(Normal50)
summary(Bootnorm50)
sd(Normal50)
sd(Bootnorm50)



#Exp20 to Bootexp20

q = summary(Exp20)



par(mfrow = c(3,2))
hist(Bootexp20,main = "Histogram of Exponential Bootstrap\n n = 5000",col=6)
boxplot(Exp20,main="Boxplot Exponential\n n = 20" ,horizontal=TRUE, xlab=  "Min.  1st Qu.  Median  Mean  3rd Qu.  Max.    sd.\n  0.0649  0.4487  0.8159  1.3790  2.2730  4.6750  1.346  " ,col=5, notch = TRUE)
boxplot(Bootexp20,main="Boxplot Exponential Bootstrap\n n=5000",horizontal=TRUE,  xlab=  "Min. 1st Qu.  Median    Mean 3rd Qu. Max.\n 0.0649  0.4487  0.8159  1.3790  2.2730  4.6750" ,col=6, notch = TRUE)
qqnorm(Exp20,col=5)
qqline(Exp20)
qqnorm(Bootexp20,col=6)
qqline(Bootexp20)

summary(Exp20,digits=3)
summary(Bootexp20, digits = 3)
sd(Exp20)
sd(Bootexp20)





par(mfrow = c(2,2))
hist(Exp20,xlab = 'n=20')
hist(bootexp20,xlab = '5000 Sample Bootstrap n=20')
hist(Exp50,xlab = 'n=50')
hist(bootexp50,xlab = '5000 Sample Bootstrap n=50')









summary(Exp20)
summary(Exp50)
sd(Exp20)
sd(Exp50)

summary(bootexp20)
summary(bootexp50)
sd(bootexp20)
sd(bootexp50)

summary(Exp50)
summary(bootexp50)
sd(Exp50)
sd(bootexp50)

library(plyr)
library(reshape2)
library(plotly)

help(geom_boxplot)
hist(Exp50)
boxplot(Exp50, 
        main ='Exp. Distribution n = 50')
qqnorm(Exp50)
qqline(Exp50)
help("boxplot")
help("box")


summary(Normal20)
summary(bootnorm20)
summary(Normal50)
summary(bootnorm50)
summary(Exp20)
summary(boot)
summary(Exp50)


summary(Exp50)
sd(Exp50)
summary(bootnorm50)
sd(bootnorm50)
par(mfrow = c(1,3))
hist(Exp50)
boxplot(Exp50)
qqnorm(Exp50)
qqline(Exp50)
hist(bootnorm50)
boxplot(bootnorm50)
qqnorm(bootnorm50)
qqline(bootnorm50)
par(mfrow = c(1,2))
hist(Exp50)
hist(bootnorm50)
boxplot(Exp50)
boxplot(bootnorm50)
qqnorm(Exp50)
qqline(Exp50)
qqnorm(bootnorm50)
qqline(bootnorm50)



##
summary(Normal20)
sd(Normal20)
summary(bootnorm20)
sd(bootnorm20)
par(mfrow = c(1,3))
hist(Normal20)
boxplot(Normal20)
qqnorm(Normal20)
qqline(Normal20)
par(mfrow = c(1,3))
hist(bootnorm20)
boxplot(bootnorm20)
qqnorm(bootnorm20)
qqline(bootnorm20)
par(mfrow = c(1,2))
hist(Normal20)
hist(bootnorm20)
boxplot(Normal20)
boxplot(bootnorm20)
qqnorm(Normal20)
qqline(Normal20)
qqnorm(bootnorm20)
qqline(bootnorm20)







