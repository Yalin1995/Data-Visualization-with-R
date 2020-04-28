rm(list=ls())
setwd("G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\02Bivariate Regression Analysis")  
library(foreign); library(car)
myPower <- read.spss("DallasTempPower.sav", to.data.frame= TRUE)

myPower$powPerDay <- myPower$kWhBill/myPower$DaysBill # calculate kWh per day

## Exploration
summary(myPower)
boxplot(powPerDay~Month, data=myPower) 
abline(h=mean(myPower$powPerDay, na.rm=TRUE), lty=5, col="red")

boxplot(myPower[, c("MinTemp","AveTemp","MaxTemp")])
hist(myPower$AveTemp, breaks=seq(40,100, by=5))

## Remove observations with NA's. Common mistake: NA in the data
myPower <- na.omit(myPower)

## Check normality (compare to myPower$AveTemp)
e1071::skewness(myPower$powPerDay, na.rm=TRUE)
car::qqPlot(myPower$powPerDay)
shapiro.test(myPower$powPerDay)                     # compared to the shapiro test
ks.test(myPower$powPerDay, pnorm,                   # the ks test has not as much power
        mean=mean(myPower$powPerDay), sd=sd(myPower$powPerDay))

## Find Box-Cox lambda
symbox(~powPerDay, data=myPower)                        # explore different lambda parameters
summary(powerTransform(lm(powPerDay~1, data=myPower)))  # test indicates log-transformation sufficient
lambda <- powerTransform(lm(powPerDay~1, data=myPower))$lambda

## Box-Cox transform powPerDay
myPower$bc.powPerDay <- car::bcPower(myPower$powPerDay, lambda=lambda)
e1071::skewness(myPower$bc.powPerDay)
car::qqPlot(myPower$bc.powPerDay)
shapiro.test(myPower$bc.powPerDay)                   
ks.test(myPower$bc.powPerDay, pnorm, 
        mean=mean(myPower$bc.powPerDay), sd=sd(myPower$bc.powPerDay))

## scatterplot with loess smoother
scatterplot(powPerDay~AveTemp, data=myPower)

## Simultaneously transform a set of variables 
## by estimating a vectors of lambdas simultaneously
summary(lambda <- powerTransform(lm(cbind(powPerDay,AveTemp)~1, data=myPower)))

myPower <- data.frame(myPower,                      # add transformed variables to myPower
                      bcPower(cbind(myPower$powPerDay,myPower$AveTemp), coef(lambda, round=T)))

##
## Handling variables with negative values
##

## Example of the z-Gamma transformation
zGamma <- function(x, gamma){(x+sqrt(x^2+gamma^2))/2}
x <- seq(-2,4, by=0.1)
gamma <- 3
zx <- zGamma(x, gamma)
plot(x,zx, type="l", xlab="X with negative values", ylab="Transformed z(x)",
     main="Box-Cox Family with Negative Values and Gamma=1")

## Simulate an positvely skewed distribution with small negative value
x <- rbeta(100, shape1=2, shape2=5)-0.1
hist(x, main="Beta distribution with rbeta(100, shape1=2, shape2=5)-0.1")

## Use powerTransform with negative values
summary(lambda <- powerTransform(x~1, family="bcnPower"))
coef(lambda)

x <- sort(x)
zx <- zGamma(x,coef(lambda)[2])
plot(x,zx, type="l", xlab="X with negative values", ylab="Transformed z(x)",
     main="Box-Cox Family with Negative Values and Gamma=0.713883")

x.bcn <- bcnPower(x, lambda=coef(lambda)[1], gamma=coef(lambda)[2])
hist(x.bcn, main="Box-Cox transformation with Negative Values")

