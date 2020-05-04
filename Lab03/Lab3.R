y <- matrix(c(3,6,3,6,3,9),ncol=1)
x0 <- rep(1,6) 
x1 <- c(1,3,1,3,1,5)

X <- cbind(x0,x1)  

toyDf <- data.frame(y,X)                  # Just look at the data
toyDf
toy.lm <- lm(y~x1,data=toyDf)
summary(toy.lm)

tXX<- t(X)%*%X                            # cross-poduct matrix
tXXInv <- solve(tXX)                      # Calculate the inverse
(b <- tXXInv%*%t(X)%*%y)
  
X <- matrix(c(1,1,1,1,3,5),nrow = 3)
W <- matrix(c(3,0,0,0,2,0,0,0,1),nrow = 3,ncol = 3)
y <- matrix(c(3,6,9),ncol=1)
(b <- (X %>% t() %*% W %*% X) %>% solve() %*% (X %>% t()) %*% W %*% y)  

y <- matrix(c(8,6,4,1,3,2,9,5,7),ncol=1)
X <- as.factor(c(rep('A',3),rep('B',3),rep('C',3)))
df <- data.frame(y,X)
contr.sum(df$X, contrasts = T)
# contrasts(df$X,contrasts = "contr.treatment")
# ?contrasts
row1 <- rep(1,9)
row2 <- c(rep(1,3),rep(0,6))
row3 <- c(rep(0,3),rep(1,3),rep(0,3))
row4 <- c(rep(0,6),rep(1,3))
row5 <- c(rep(1,3),rep(0,3),rep(-1,3))
row6 <- c(rep(0,3),rep(1,3),rep(-1,3))
row7 <- c(rep(1,3),rep(-1,3),rep(0,3))
row8 <- c(rep(0,3),rep(-1,3),rep(1,3))
(x1 <- cbind(row1,row2,row3))
(x2 <- cbind(row1,row2,row4))
(x3 <- cbind(row1,row5,row6))
(x4 <- cbind(row1,row7,row8))
model.1 <- lm(y~x2)
summary(model.1)
model.1
?contr.sum

data("CPS1985",package="AER")
rownames(CPS1985) <- 1:nrow(CPS1985)
scatterplotMatrix(~log(wage)+education+age+experience,data =CPS1985,
                  main="Relationship between log(wage) and a set of independent variables",
                  pch=1, smooth=list(span = 0.35,lty.smooth=1, col.smooth="red", col.var="red"),
                  regLine=list(col="green"))

cor(CPS1985$age,CPS1985$experience)
str(CPS1985$age)
str(CPS1985$experience)


model.2 <- lm(log(wage)~education+experience,data =CPS1985)
summary(model.2)
vif(model.2)
cor(CPS1985$education,CPS1985$experience)

model.3 <- lm(log(wage)~education+experience+age,data =CPS1985)
summary(model.3)
vif(model.3)

model.4 <- lm(log(wage)~experience+age,data =CPS1985)
summary(model.4)


model.4 <- lm(log(wage)~education+experience+gender+occupation+union,data =CPS1985)
model.5 <- lm(log(wage)~education+experience+gender+union,data =CPS1985)
anova(model.5,model.4)
summary(model.4)

car::residualPlots(model.4)

boxplot(log(wage)~occupation, data=CPS1985, main="Wage across occupation")
table(CPS1985$occupation)

model.6 <- update(model.4, .~.+I(experience^2))
summary(model.6)
car::qqPlot(model.6)
car::influenceIndexPlot(model.6)
car::avPlots(model.6)
df_new<- CPS1985[-c(171,200),]
summary(CPS1985$wage)
sort(CPS1985$wage)
tapply(CPS1985$wage,CPS1985$occupation,summary)
getwd()
df1 <- foreign::read.dta('G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\Lab03\\card.dta')
df2 <- subset(df1, !is.na(df1$lwage))
scatterplotMatrix(~lwage+educ+age, data=df2,
                  pch=1, smooth=list(span = 0.35,lty.smooth=1, col.smooth="red", col.var="red"),
                  regLine=list(col="green"))

cig.iv <-ivreg(lwage~educ+age|
                 age+nearc2+nearc4, data=df2)
summary(cig.iv, diagnostics=T)
cor(df2$educ,df2$nearc2)
cor(df2$educ,df2$nearc4)

cor((df2$lwage - fitted(cig.iv)),df2$nearc2)
