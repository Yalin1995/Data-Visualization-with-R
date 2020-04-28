rm(list=ls())
library(car)
setwd("G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\02Bivariate Regression Analysis")                 # Change working directory

Concord <- foreign::read.spss("Concord1.sav",to.data.frame=TRUE)

help(lm)
reg01 <- lm(water81~income, data=Concord)
class(reg01)

help(summary.lm)
summary(reg01)
hist(resid(reg01))
round(sum(resid(reg01)),14)
coef(reg01)

help(confint)
cbind("Coef"=coef(reg01), confint(reg01, level=0.95))

help(predict.lm)
predDf <- data.frame(income=min(Concord$income):max(Concord$income))   # data-frame for independent vars
predDf <- data.frame(predDf,
                     predict(reg01, newdata=predDf, 
                     interval="confidence", level=0.95))               # Line confidence interval & fit
# predDf <- data.frame(predDf,
#                      predict(reg01, newdata=predDf, 
#                              interval="prediction", level=0.95))       # Point confidence interval & fit
#                                           
head(predDf)

plot(water81~income,data=Concord)
lines(predDf$income,predDf$fit,col="red")                               # predicted value
lines(predDf$income,predDf$lwr,col="green")                             # lower confidence interval limits
lines(predDf$income,predDf$upr,col="green")                             # upper confidence interval limits
abline(h=mean(Concord$water81),v=mean(Concord$income),lty=3)            # Regression line goes thru the means

## Fancy(?) scatterplot with loss curve and marginal box-plots
car::scatterplot(water81~income, data=Concord, main="Concord Households: Water Consumption against Income" )
plot(resid(reg01)~fitted(reg01))
abline(h=0)

