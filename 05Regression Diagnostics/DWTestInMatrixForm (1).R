##
## The DW-test for serial AC
##
n <- 10
x <- rnorm(n, mean=2)                      # independent random values =>neutral AC value
x <- sort(x)                               # similar pattern => positively AC value
x[seq(1,n,by=2)] <- -x[seq(1,n,by=2)]      # alternating pattern => negative AC value

M1 <- diag(1, n)-matrix(1/n,nrow=n,ncol=n) # projection matrix

e <- x - mean(x)                           # residuals = x - expected value
e <- M1 %*%x                               # same using the projection matrix

diffMat <- diag(-1, nrow=n-1, ncol=n)      # build the difference matrix 
for (i in 1:(n-1)) diffMat[i,i+1] <- 1

diffMat
(dwMat <- t(diffMat) %*% diffMat)          # see lab 1


(diffE <- diffMat %*% e)                   # calculate e_t - e_t-1

(DW1 <- (t(diffE)%*%diffE) / (t(e)%*%e))
(DW2 <- (t(e)%*%dwMat%*%e) / (t(e)%*%e))

