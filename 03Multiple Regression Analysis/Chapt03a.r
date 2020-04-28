rm(list=ls())  # start clean
library(car); library(effects)

setwd("G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\03Multiple Regression Analysis")
concord <- foreign::read.spss("Concord1.sav",to.data.frame=T)                           

car::scatterplot(water81~income, data=concord, xlab="Income in $1,000",ylab="Wate consumption in 1981 in ft^2")

#####################################################
## Transition from bivariate to multivariate analysis
#####################################################
mod1.lm <- lm(water81 ~ income, data=concord)  # adding "data" statement avoids attaching concord
summary(mod1.lm)

mod2.lm <- lm(water81 ~ income + water80, data=concord)
summary(mod2.lm)

#################################
## Demonstration: Partial Effects
#################################
y.x2 <- residuals(lm(water81 ~ water80, data=concord))                        # Controling for water80
plot(concord$water81,y.x2)
cor(concord$water81,y.x2)
plot(concord$water80,y.x2)
cor(concord$water80,y.x2)

x1.x2 <- residuals(lm(income ~ water80, data=concord))

cat("Sum of residuals for Water81|Water80:",sum(y.x2),"\n")                   #sum of residuals = 0
cat("Sum of residuals for Income|Water80: ",sum(x1.x2),"\n")

scatterplot(y.x2~x1.x2, xlab=bquote(hat(bold(e))["Income|Water80"]),          # see Fig 3.1
            ylab=bquote(hat(bold(e))["Water81|Water80"]))   
summary(lm(y.x2 ~ x1.x2-1))                                                   # compare with coefficients of mod2.lm

y.x1 <- residuals(lm(water81 ~ income, data=concord))                         # Controling for income
x2.x1 <- residuals(lm(water80 ~ income, data=concord))
scatterplot(y.x1~x2.x1, xlab=bquote(hat(bold(e))["Water81|Income"]),          # see Fig 3.2
            ylab=bquote(hat(bold(e))["Water80|Income"]))   
summary(lm(y.x1 ~ x2.x1-1))                                                   # compare with coefficients of mod2.lm

##########################
## Seven-Variables Example
##########################
scatterplotMatrix(~water81+income+water80+educat+retire+peop81+cpeop,
                  data=concord, diagonal="density", span=.75)
summary(lm(water81~educat, data=concord))
summary(lm(water81~retire, data=concord))

# effect of the factor retire on the metric variables
retire.lm <- lm(cbind(water81,income,water80,educat,peop81,cpeop)~retire, data=concord)
summary(retire.lm)

# full model
mod3.lm <- lm(water81 ~ income+water80+educat+retire+peop81+cpeop, data=concord)
summary(mod3.lm)
cbind(Estimate=coef(mod3.lm), confint(mod3.lm))                        # print confidence interval for parameter estimate

# only significant variables (exclude cpeop)
summary(lm(water81 ~ income+water80+educat+retire+peop81, data=concord))

# intercept model
summary(lm(water81~1, data=concord))
mean(concord$water81)


###############
## Beta weights
###############
concord$retireDummy <- as.numeric(concord$retire)-1                     # convert factor to metric. retired:"yes"=1,"no"=0 
mod4.lm <- lm(scale(water81) ~ scale(income)+scale(water80)+scale(educat)+      # dirty approach
                               scale(retireDummy)+scale(peop81)+scale(cpeop), data=concord)
summary(mod4.lm)

concordNew <- concord[ ,sapply(concord,is.numeric)]                     # Remove non-numeric variables
concordScale <- as.data.frame(scale(concordNew))                        # apply z-transformation with scale function
                                                                        # dataframe concordScale holds the transformed values
mod4.lm <- lm(water81 ~ -1+income+water80+educat+retireDummy+peop81+cpeop, data=concordScale)
summary(mod4.lm)                                                        # display beta-values. Why is the intercept=0 ?

# coefficent plot useful for beta weights because the parameters are on the same scale
library (coefplot)                                                      # needs to be downloaded
coefplot(mod4.lm)

################################################
## Partial F-test: H0: beta_RETIRE=beta_CPEOP=0
################################################
mod5.lm <- lm(water81 ~ water80+income+educat+peop81, data=concord)
summary(mod5.lm)
anova(mod5.lm,mod3.lm)   # Compare both models
Anova(mod3.lm)
###########################################
## Stepwise variable selection from mod3.lm
###########################################
null.lm <- lm(water81~1, data=concord)
mod6.step <- step(null.lm,
                  scope= ~income+water80+educat+retire+peop81+cpeop,
                  direction="forward")
summary(mod6.step)

##########################################
## Alternative stepwise specification
##########################################
mod7.step <- step(mod3.lm, scope=list(lower=null.lm, upper=mod3.lm), direction="backward")
summary(mod7.step)

#############################################
## Conditional Effects: HAM pp 154-163 (skip)
#############################################
library(effects)

concord$clogpeop <- log(concord$peop81 / concord$peop80)
mod08.lm <- lm(log(water81)~log(income)+log(water80)+educat+retire+log(peop81)+clogpeop, data=concord)
summary(mod08.lm)

## All effects at mean level of remaing variavles
plot(allEffects(mod08.lm, transformation=list(link=log, inverse=exp)), ylab="water81")

## get value ranges
summary(concord)
summary(log(concord$water80))
summary(log(concord$peop81))
## Income effect for a low consumer profile
plot(effect("log(income)", mod08.lm, given.values=c("log(water80)"=7.3, "educat"=18, "log(peop81)"=0.7),
            transformation=list(link=log, inverse=exp) ), 
     ylim=c(log(1000),log(6000)), ylab="water81", 
     main="Income Effect of Low Water Consumer")

## Income effect for an average consumer profile
plot(effect("log(income)", mod08.lm, given.values=c("log(water80)"=7.7, "educat"=14, "log(peop81)"=1.0),
            transformation=list(link=log, inverse=exp) ), 
     ylim=c(log(1000),log(5000)), ylab="water81", 
     main="Income Effect of Average Water Consumer")


## Income effect for a high consumer profile
plot(effect("log(income)", mod08.lm, given.values=c("log(water80)"=8.3, "educat"=10, "log(peop81)"=1.4),
            transformation=list(link=log, inverse=exp) ), 
     ylim=c(log(1000),log(5000)), ylab="water81", main="Income Effect of High Water Consumer")





