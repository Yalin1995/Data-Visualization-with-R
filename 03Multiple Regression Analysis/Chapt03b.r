library(car)
rm(list=ls(all=TRUE))
setwd("G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\03Multiple Regression Analysis")

wells <- foreign::read.spss("wells.sav",to.data.frame=TRUE)
names(wells)
summary(wells)
##
## Explore Coding Scheme of Factors
##
class(wells$deep)                                      # Deep is a factor, i.e., categorical variable
wells$deep                                             # Character without quotes
                                                       # plus details on levels
contrasts(wells$deep)                                  # See coding of factor

contrasts(wells$deep) <- "contr.sum"                   # Change to 1,0,-1 coding
contrasts(wells$deep)

contrasts(wells$deep) <- "contr.treatment"             # Change back to 0,1 coding
contrasts(wells$deep)

##
## Prepare data for analysis
##
wells1 <- na.omit(wells)                               # drop observation 18 with missing values for Chlor
wells1$logChlor <- log(wells1$chlor)                   # Transform to natural logarithm
wells1$logDist <- log(wells1$droad)                    # Transform to natural logarithm
wells1$deepdum <- as.numeric(unclass(wells1$deep))-1   # Convert factor to numeric 0/1 dummy
wells1$deepdum
scatterplot(logChlor~logDist|deep, smooth=F, data=wells1)

attach(wells1)                                         # attach allows to access the variables in a df directly
levels(deep)
## First look
wellSymbol <- ifelse(deep==levels(deep)[1],15,16)      # Symbols & colors for well type
wellCol <- ifelse(deep==levels(deep)[1],"red","blue")  
plot(droad,chlor, log="xy", pch=wellSymbol, col=wellCol)
legend("topright",legend=c("shallow","deep"), 
       title="Well Type:", col=c("red","blue"),pch=c(15,16))

##
## Run several models with dummy variables and interaction terms
##
mod0 <- lm(logChlor ~ deep -1)                         # One way analysis of variance
summary(mod0)                                          # Suppressing intercept gives mean levels
model.matrix(mod0)

mod1 <- lm(logChlor ~ deep)                            # One-way analysis of variance
summary(mod1)
model.matrix(mod1)

mod2 <- lm(logChlor ~ logDist)                         # Standard Regression
summary(mod2)
model.matrix(mod2)

mod3 <- lm(logChlor ~ deep + logDist)                  # Regression with intercept dummy
summary(mod3)
model.matrix(mod3)

mod4 <- lm(logChlor ~ logDist + logDist:deep)          # Regression with slope dummy
summary(mod4)
model.matrix(mod4)

mod5 <- lm(logChlor ~ deep + logDist + logDist:deep)   # Regression with intercept and slope dummy
summary(mod5)
model.matrix(mod5)

mod5 <- lm(logChlor ~ deep*logDist)                    # Identical to mod5: main effects and their interaction
summary(mod5)

anova(mod2,mod5)                                       # Are main and interaction effect jointly significant?
##
## Plot the different models
##
plot(deepdum,logChlor,pch=wellSymbol,col=wellCol)      # just dummy no interacton
abline(mod1)
legend("topright",legend=c("shallow","deep"), title="Well Type", col=c("red","blue"),pch=c(15,16))

plot(logDist,logChlor,pch=wellSymbol,col=wellCol)      # just distance not interaction
abline(mod2)
legend("topright",legend=c("shallow","deep"), title="Well Type", col=c("red","blue"),pch=c(15,16))

plot(logDist,logChlor,pch=wellSymbol,col=wellCol)      # intercept dummy
abline(mod3$coef[1],mod3$coef[3],col="blue")
abline(mod3$coef[1]+mod3$coef[2],mod3$coef[3],col="red")
legend("topright",legend=c("shallow","deep"), title="Well Type", col=c("red","blue"),pch=c(15,16))

plot(logDist,logChlor,pch=wellSymbol,col=wellCol)      # slope dummy
abline(mod4$coef[1],mod4$coef[2],col="red")
abline(mod4$coef[1],mod4$coef[2]+mod4$coef[3],col="blue")
legend("topright",legend=c("shallow","deep"), title="Well Type", col=c("red","blue"),pch=c(15,16))

mod5 <- lm(logChlor ~ deep + logDist + logDist:deep) 
coef(mod5)
plot(logDist,logChlor,pch=wellSymbol,col=wellCol)      # intercept and slope dummy
abline(mod5$coef[1],mod5$coef[3],col="red")
abline(mod5$coef[1]+mod5$coef[2],mod5$coef[3]+mod5$coef[4],col="blue")
legend("topright",legend=c("shallow","deep"), title="Well Type", col=c("red","blue"),pch=c(15,16))

##
## Alternative: Run two independent models on subsets of the data
##
shallowDf <- subset(wells1, deepdum==0) 
deepDf <- subset(wells1, deepdum==1)

shallow.lm <- lm(logChlor~logDist,data=shallowDf)
summary(shallow.lm)
deep.lm <- lm(logChlor~logDist,data=deepDf)
summary(deep.lm)

plot(logDist,logChlor,pch=wellSymbol,col=wellCol)
abline(shallow.lm, col="red")
abline(deep.lm, col="blue")
legend("topright",legend=c("shallow","deep"), title="Well Type", col=c("red","blue"),pch=c(15,16))

##############################################################################
## Cohen Radon Data-set: Analysis of Variance                               ##
## Not lab or test relevant                                                 ##
##############################################################################
cohen <- foreign::read.spss("CohenRadon.sav",to.data.frame=TRUE)
cohen <- na.omit(cohen)                 # exclude records with missing information

cohen$radFac <- cut(cohen$radon,        # recode metric "radon" into factor
                 breaks=c(min(cohen$radon),1.5,2.4,max(cohen$radon)+1),
                 labels=c("low","mid","high"))

## Mean cancer rate in each locale
tapply(cohen$cancer, cohen$locale, mean)

## Perform analysis in dummy coding scheme (this is the default setting)
## Global assignment using options (first parameter for factors, second for ordered factors)
options(contrasts=c("contr.treatment","contr.poly")) # Change to dummy coding scheme

levels(cohen$radFac)
contrasts(cohen$radFac)

levels(cohen$locale)
contrasts(cohen$locale)  # First level "R.Prong" reference

lm.dummyRP<- lm(cancer~locale,data=cohen)
summary(lm.dummyRP)
model.matrix(lm.dummyRP)

cohen$locale <- relevel(cohen$locale,ref="Control") # Now "Control" becomes the reference
contrasts(cohen$locale) 

lm.dummyCtr<- lm(cancer~locale,data=cohen)
summary(lm.dummyCtr)
model.matrix(lm.dummyCtr)

## Perform analysis in effect coding scheme
options(contrasts=c("contr.sum","contr.poly")) # Change to effect coding scheme

contrasts(cohen$locale)                        # "Fringe" is reference
contrasts(cohen$radFac)
lm.effectFringe <- lm(cancer~locale,data=cohen)
summary(lm.effectFringe)
model.matrix(lm.effectFringe)

## Interaction among factors
interFact <- interaction(cohen$locale,cohen$radFac)
levels(interFact)
interFact

## Twoway Analysis of variance with interaction term
lm.effectTwoway <- lm(cancer~locale+radFac+locale:radFac,data=cohen)
summary(lm.effectTwoway)
Anova(lm.effectTwoway)
model.matrix(lm.effectTwoway)

## Analysis of Covariance
lm.effectTwoway <- lm(cancer~locale+radon,data=cohen)
summary(lm.effectTwoway)
Anova(lm.effectTwoway)

