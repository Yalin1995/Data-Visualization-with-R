rm(list=ls())
library(car); library(effects)
hprice2 <- foreign::read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/hprice2.dta")
##
## hprice2: 506 communities in the Boston area
## Relevant variables:
##    price:   median home price in community
##    crime:   crime rate (?)
##    nox:     nitrogen oxide in the air
##    dist:    weighted distance to five employment centers
##    rooms:   average number of in houses in the community
##    stratio: Student-teacher ratio of schools in the community
##    radial:  ???
##    proptax: property tax in community per $1000 home value (?)
##
summary(hprice2)
summary(powerTransform(cbind(price,crime,nox,dist,rooms,stratio)~1, data=hprice2))
scatterplotMatrix(~price+crime+nox+dist+rooms+stratio, data=hprice2)
scatterplotMatrix(~log(price)+log(crime)+log(nox)+log(dist)+rooms+stratio, data=hprice2)

## Zero model
mod0 <- lm(price~nox+dist+rooms+stratio, data=hprice2)
summary(mod0)

mod0 <- lm(price~nox+dist+rooms+I(rooms^2)+stratio, data=hprice2)
summary(mod0)

library(effects)
mod0.eff <- allEffects(mod0, xlevels=list(rooms=3:9))
plot(mod0.eff, "rooms", main="Non-linear effect of # of rooms")


## Base model
mod1 <- lm(log(price)~log(crime)+log(nox)+log(dist)+rooms+stratio, data=hprice2)
summary(mod1)

## Quadratic effects
mod2 <- lm(log(price)~log(crime)+log(nox)+log(dist)+rooms+I(rooms^2)+stratio, data=hprice2)
summary(mod2)

## anova test
anova(mod1,mod2)

## Explore non-linear effect
## Turning point: d(lprice)/d(rooms)=0 => 0.672/(2*0.072)=4.667

mod2a.eff <- allEffects(mod2, xlevels=list(rooms=3:9))
plot(mod2a.eff, "rooms", ylab="log(price)", main="Non-linear effect of # of rooms")


mod2b.eff <- allEffects(mod2, xlevels=list(rooms=3:9),
                       transformation=list(link=log, inverse=exp))
plot(mod2b.eff, "rooms", ylab="price", main="Non-linear effect of # of rooms")


