rm(list=ls(all=TRUE))  # start clean
library(car)
setwd("G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\05Regression Diagnostics")

ampPhase <- function(mod, cosStr, sinStr, period){
  #############################################################
  ## Objective: Calculates amplitude and phase shift based on
  ##            sin and cos terms. Evaluates standard errors
  ##            and confidence intervals by car::delta-method
  ## Input:
  ## mod        model object (lm or glm)
  ## cosStr     name of cos-term as string in the model object
  ## sinStr     name of sin-term as string in the model object
  ## period     numeric length of a full period in # of obs.
  #############################################################
  
  if ( !is.character(cosStr) ) stop("cosStr must be entered as string")
  if ( !is.character(sinStr) ) stop("sinStr must be entered as string")
  
  ## Standard error of amplitude
  amplitudeSe <- car::deltaMethod(mod, paste0("sqrt(",cosStr,"^2+",sinStr,"^2)"))
  
  ## Standard error of phase shift
  atanStr <- paste0("atan(",sinStr,"/",cosStr,")")
  
  if (mod$coefficients[cosStr] > 0 & mod$coefficients[sinStr] > 0)
    phaseSe <- car::deltaMethod(mod, paste0(atanStr,"*",period,"/(2*pi)"))
  if (mod$coefficients[cosStr] < 0)
    phaseSe <- car::deltaMethod(mod, paste0("(",atanStr,"+pi)*",period,"/(2*pi)"))
  if (mod$coefficients[cosStr] > 0 & mod$coefficients[sinStr] < 0)
    phaseSe <- car::deltaMethod(mod,paste0("(",atanStr,"+2*pi)*",period,"/(2*pi)"))
  
  fourierResult <- rbind(amplitudeSe,phaseSe)
  row.names(fourierResult) <- c("Amplitude:","Phase Shift:")
  return(fourierResult)
}


##
## Build a periodic time series to explore its properties
##
yrs <- 10               # number of years
t <- seq(1,yrs*12,by=1) # monthly index
lambda <- 1/12          # frequency for annual cycle

amplitude <- 4
phase <- 6*lambda*2*pi                          # phase shift in months. Phase in [0,2*pi] 
y.t <- amplitude * cos(2*pi*lambda*t - phase)   # cyclic model. Remember cos(0)=1

plot(y.t~t,type="b", lwd=2, xlim=c(0,yrs*12+1))
abline(h=0, v=1, lwd=1)                                 # reference line
abline(h=c(-amplitude,amplitude), col="green", lty=5)   # amplitude
abline(v=seq(1,yrs*12+1,by=12), lty=2, col="red")       # mark January of each year

##
## Standard Regression Diagnositcs
##
sandusky <- foreign::read.spss("SanduskyTemperature.sav", use.value.labels = TRUE, to.data.frame = TRUE)

## Evaluate monthly cycle and variance heterogeneity
boxplot(avg7447~month, data=sandusky, ylab="Average Temperature in Fahrenheit", main="Sandusky Climate 1990-1999")
plot(avg7447~time.idx, data=sandusky, main="Monthly Temperature Variation at Sandusky, Ohio, from 1990 to 1999",
     xlab="Sequence in Months", ylab="Temperature", type="l")

## Generate harmonic variables and add them to the data-frame
sandusky$r.cos <- cos(sandusky$time.idx/12*2*pi)
sandusky$r.sin <- sin(sandusky$time.idx/12*2*pi)

fourier1.lm <- lm(avg7447~time.idx+r.cos+r.sin, data=sandusky)   # Fouier regression with 2 wave parameters
summary(fourier1.lm,cor=T)                                       # Summary with correlation among Parameters
vif(fourier1.lm)                                                 # Variance inflation factors (reported in variance NOT std)
round(vcov(fourier1.lm),2)                                       # covariance among estimated parameters

## Get amplitude and phase-shift in months (peak of the cycle)
round(ampPhase(fourier1.lm, "r.cos", "r.sin", 12), 2)

## Fixed effect panel model
month.lm <- lm(avg7447~time.idx + month, data=sandusky)         # using month-indicators with 12 levels (not a nested model)
summary(month.lm)                                               # adjusted R^2 is smaller than that for fourier.lm

## Diagnostic plots
qqPlot(fourier1.lm, id.n=4)                                     # Test of normality based on t-distribution
avPlots(fourier1.lm, id.n=4)                                    # Partial effects plots
residualPlots(fourier1.lm)                                      # Residual plots

## Update the model by adding I(time.idx^2)
fourier2.lm <- update(fourier1.lm, .~.+I(time.idx^2))
summary(fourier2.lm)
anova(fourier1.lm, fourier2.lm)
residualPlots(fourier2.lm)                                      # recheck for non-linearity

## Get residuals
resid <- residuals(fourier2.lm)
std.resid <- rstandard(fourier2.lm)
student.resid <- rstudent(fourier2.lm)

## Other diagnositic measures
dfbeta.values <- dfbetas(fourier2.lm)
boxplot(dfbeta.values, main="dfBetas values")
cook.values <- cooks.distance(fourier2.lm)
boxplot(cook.values, main="cook-values", id.n=2)
leverage.values <- hatvalues(fourier2.lm)
boxplot(leverage.values, main="leverage values")

## Explore outlier statisitics => open df outliers in editor and sort by outliers
outliers <- data.frame(sandusky,student.resid,dfbeta.values,
                       cook.values,leverage.values)
outlierTest(fourier2.lm)

car::influencePlot(fourier2.lm, id.n=4)
car::influenceIndexPlot(fourier2.lm, id.n=4)    # Be careful: inspect scale of Bonferroni p-values

## Proportional Leverage Plot
radius <- abs(dfbeta.values[,2])*20
avPlots(fourier2.lm, terms= ~.-r.cos-r.sin-I(time.idx^2), cex=radius)   # easy specification

## Plot prediction and confidence intervals using spline smoother
pred.wave <- data.frame(predict(fourier2.lm, interval = "prediction"))
plot(avg7447~time.idx, data=sandusky, main="Monthly Temperature Variation at Sandusky, Ohio, from 1990 to 1999",
     xlab="Sequence in Months", ylab="Temperature", type="n")
lines(spline(sandusky$time.idx, pred.wave$fit), type="l", lwd=2, col="salmon3" )
lines(spline(sandusky$time.idx, pred.wave$upr), type="l", lty=3, lwd=1, col="salmon1" )
lines(spline(sandusky$time.idx, pred.wave$lwr), type="l", lty=3, lwd=1, col="salmon1")
points(sandusky$time.idx, sandusky$avg7447, pch=20, col="seagreen")
abline(h=mean(sandusky$avg7447),lty="longdash")
abline(v=0:9*12+1,lty="longdash")

