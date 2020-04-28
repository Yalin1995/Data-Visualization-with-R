rm(list=ls())
library(car); library(effects)
attend <- foreign::read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/attend.dta")
##
## Key variables:
## stndfnl: 	Standardized outcome on final exam
## atndrte: 	Percentage of class attendence
## priGPA:  	Prior college grade point average
## ACT:     	American College Testing score
##
summary(attend)

## Base model
scatterplotMatrix(~stndfnl+atndrte+priGPA+ACT, data=attend)
mod1 <- lm(stndfnl~atndrte+priGPA+ACT, data=attend)
summary(mod1)
model.matrix(mod1)

## With interaction. Notice the "*" in the formula
mod2 <- lm(stndfnl~atndrte*priGPA+ACT, data=attend)
summary(mod2)
model.matrix(mod2)

## Partial F-test (here equal to the t-test for the interaction term)
anova(mod1,mod2)

## Effect of attendence rate at average priGPA
b <- coef(mod2)
mean(attend$priGPA)
cat("\nPartial effect of atndrte for priGPA=2.59:")
b["atndrte"]+mean(attend$priGPA)*b["atndrte:priGPA"]

## Test partial effect at priGPA=mean(attend$priGPA)
linearHypothesis(mod2, c("atndrte+2.59*atndrte:priGPA"))

plot(allEffects(mod2))
plot(Effect(c("atndrte","priGPA"), mod2, 
            xlevels=list(priGPA=seq(1.5, 3.5, by=0.5))))
