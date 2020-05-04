library(AER)
data("CPS1985")
setwd('G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\Lab01')
symbox(~wage, data=CPS1985)
summary(powerTransform(lm(wage~1, data=CPS1985)))
534/
hist(car::bcPower(CPS1985$wage, lambda=1),breaks = 12,main = 'lambda = 1')
hist(car::bcPower(CPS1985$wage, lambda= -0.0658 ),breaks = 12,main = 'lambda = -0.0658')
hist(car::bcPower(CPS1985$wage, lambda= -1.5 ),breaks = 12,main = 'lambda = -1.5')
hist(car::bcPower(CPS1985$wage, lambda=-0.0658),breaks = c(0,5,10,15,20,25,30,35,40,45),main = 'lambda = -0.0658')
range(CPS1985$wage)
?hist
e1071::skewness(car::bcPower(CPS1985$wage, lambda=-1.5))

shapiro.test(car::bcPower(CPS1985$wage, lambda=1))
shapiro.test(car::bcPower(CPS1985$wage, lambda=-0.0658))
shapiro.test(car::bcPower(CPS1985$wage, lambda=-1.5))
tranformed_x <- car::bcPower(CPS1985$wage, lambda=-0.0658)

ks.test(tranformed_x, pnorm, mean=mean(tranformed_x), sd=sd(tranformed_x))
?ks.test
Concord <- foreign::read.spss("Concord1.sav ", to.data.frame= TRUE)
Concord <- na.omit(Concord)
summary(lambda <- powerTransform(lm(cbind(water79,water80,water81)~1, data=Concord)))

Concord <- data.frame(Concord,                      # add transformed variables to myPower
                      bcPower(cbind(Concord$water79,Concord$water80,Concord$water81), coef(lambda, round=T)))
Concord$Z2.0.18.1
range(Concord$water80)
hist(Concord$Z1.0.17,breaks = 12,main = paste('Water79 , Skewness =',round(e1071::skewness(Concord$Z1.0.17),2)))
hist(Concord$Z2.0.18,breaks = 12,main = paste('Water80 , Skewness =',round(e1071::skewness(Concord$Z2.0.18),2)))
hist(Concord$Z3.0.33,breaks = 12,main = paste('Water81 , Skewness =',round(e1071::skewness(Concord$Z3.0.33),2)))

range(Concord$Z2.0.18)

reg01 <- lm(wage~education, data=CPS1985)
summary(reg01)
CPS1985$education
cbind("Coef"=coef(reg01), confint(reg01, level=0.99))

predDf <- data.frame(education=min(CPS1985$education):max(CPS1985$education)) 
predDf <- data.frame(predDf,
                     predict(reg01, newdata=predDf, 
                             interval="confidence", level=0.90))
reg01$coefficients[1]
library(ggplot2)
attach(CPS1985)
q <- ggplot(CPS1985,aes(education,wage)) + geom_point() + geom_smooth(method=lm, se=TRUE,level =0.9)
q <- q + geom_vline(xintercept =mean(education),color = "red") + 
  geom_hline(yintercept =mean(wage),color = "red")
q + ggtitle('CPS1985:Education Level against Wage')
?geom_smooth
plot(wage~education,data=CPS1985)
lines(predDf$education,predDf$fit,col="red")                               # predicted value
lines(predDf$education,predDf$lwr,col="green")                             # lower confidence interval limits
lines(predDf$education,predDf$upr,col="green")  
library(car)

df_crime <- foreign::read.dbf('CampusCrime.dbf',as.is = T)
range(df_crime$crime)
range(df_crime$police)
boxplot(df_crime[, c("police","crime")])
e1071::skewness(df_crime$police, na.rm=TRUE)
e1071::skewness(df_crime$crime, na.rm=TRUE)

summary(x.lambda <- powerTransform(lm(crime~1, data=df_crime)))
summary(y.lambda <- powerTransform(lm(police~bcPower(df_crime$crime,lambda = x.lambda$lambda), data=df_crime)))


df <- data.frame(bcPower(cbind(df_crime$police,df_crime$crime), lambda = c(0,0)))
colnames(df) <- c('police','crime')
model.crime <-  lm(df$police~df$crime)
summary(model.crime)

t <- (model.crime$coefficients[2] - 1)/(0.04326)
pt(t,df =91) * 2
?dt
summary(model.crime)
model.crime$coefficients[1]
plot(x = df_crime$police,y = predict(model.crime,newdata = df_crime))
df <-  data.frame(df_crime$police, predict(model.crime,))
?linearHypothesis
linearHypothesis(model.crime,c("(Intercept) = 0.2790702 ", "df$crime = 1"))

plotBoxCox(df_crime$police,df_crime$crime,0,0)

x <- seq(from = 0,to = 5,by = 0.1)
y1 <- function(x){return(exp(-(x^1)))}
y2 <- function(x){return(exp(-(x^2)))}
y3 <- function(x){return(exp(-(x^3)))}
plot(x,y1(x),col = 'red',type = 'l')
lines(x,y2(x),col = 'blue',type = 'l')
lines(x,y3(x),col = 'green',type = 'l')
legend("topright", legend=c("exp(-x^1)", "exp(-x^2)","exp(-x^3)"),
       col=c("red", "blue",'green'), lty=1, cex=0.8)

qnorm(0.005,mean = -0.7459797 ,sd = 1.04545,lower.tail = T)
qnorm(0.005,mean = -0.7459797 ,sd = 1.04545,lower.tail = F)
?qt
(a1 <- integrate(y1, 0, Inf))
(a2 <- integrate(y2, 0, Inf))
(a3 <- integrate(y3, 0, Inf))

y11 <- function(x){x*exp(-x^1)/a1$value}
y22 <- function(x){x*exp(-x^2)/a2$value}
y33 <- function(x){x*exp(-x^3)/a3$value}

(a11 <- integrate(y11, 0, Inf))

(a22 <- integrate(y22,0,Inf))

(a33 <- integrate(y33,0,Inf))



plot(x,y33(x),col = 'red',type = 'l')
lines(x,y22(x),col = 'blue',type = 'l')
lines(x,y11(x),col = 'green',type = 'l')

y111 <- function(x){(x-a11$value)^2*exp(-x^1)/a1$value}
y222 <- function(x){(x-a22$value)^2*exp(-x^2)/a2$value}
y333 <- function(x){(x-a33$value)^2*exp(-x^3)/a3$value}

integrate(y111, -Inf, Inf)

integrate(y222,-Inf ,Inf)

integrate(y333,-Inf,Inf)

plot(x,y333(x),col = 'red',type = 'l')
lines(x,y222(x),col = 'blue',type = 'l')
lines(x,y111(x),col = 'green',type = 'l')
  