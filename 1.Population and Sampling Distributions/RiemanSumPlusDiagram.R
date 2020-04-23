rm(list=ls(all=TRUE))  # Start clean
riemanTut <- FALSE

################################################################################
## Demonstrates numerical integration to get 
## [a] the distribution function from the density function
## [b] calculate the expectation
## [c] calculate the variance
##
## Example: the exponential distribution with x >= 0 and parameter lambda
##          density <- lambda*exp(-lambda*x)
##          distribution <- 1 - exp(-lambda*x)
##          expectation  <- 1/lambda
##          variance <- 1/lambda^2
##
##          Estimator lambda <- 1/mean(x)
################################################################################

IntBoxes <- function(IntFunc,a,b,n,plotIt=TRUE){
  ## plots the Rieman summands into an existing plot
  ## Calculate the midpoint Riemann sum
  ## start value <- a
  ## end value <- b
  ## # of summands <- n
  intgrnd <- match.fun(IntFunc)
  integrand <- function(x) intgrnd(x)
  xleft <- seq(a,b-((b-a)/n),by=(b-a)/n)
  xright <- seq(a+((b-a)/n),b,by=(b-a)/n)
  ybottom <- rep(0,n-1)
  ytop <- integrand(seq(a+((b-a)/(2*n)),b-((b-a)/(2*n)),by=(b-a)/n))
  if (plotIt) rect(xleft,ybottom,xright,ytop,col="grey")      # plot summands
  RieSum <- (b-a)/n*sum(ytop)
  return(RieSum)
} #end::IntBox

  ## Parameters
  nBoxes <- 80      # Number of Summands for the Rieman sum
  lambda <- 1       # Define lambda as a global variable
  xMin <- 0         # Lower integration bound
  xMax <- 10        # Upper integraion bound. Set xMax larger for smaller lambdas 
  x <- seq(xMin,xMax,length.out=500) # Sequence of x values for plot
  
  ## Define function to be evaluated
  ExpDens <- function(x) {                     # density
    ifelse(x >= 0,lambda*exp(-lambda*x),0)
  }
  ExpDensExpect <- function(x) {               # expected value
    ifelse(x >= 0,x * ExpDens(x),0)
  }
  
  ExpDensVar <- function(x) {                   # variance
    ifelse(x >= 0,(x-1/lambda)^2 * ExpDens(x),0)
  }
if (!riemanTut){  
  ##
  ## Evaluate Integrals of the exponential distribution
  ##
  ## Distribution function with a specific range
    xCut <- xMax
    plot(x,ExpDens(x),type="n",xlab="x-value",
         ylab=bquote(paste(f(x),"  at  ", lambda %==% .(lambda))))
    abline(v=0,lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
    ExpDistrib <- IntBoxes(ExpDens,xMin, xCut, nBoxes)
    lines(x,ExpDens(x),type="l",col="red",lwd=3)
    title(main=bquote(paste("Distribution: ",integral(lambda%.%exp(-lambda%.%x)%.%dx, .(xMin), .(xMax)) %~~% .(round(ExpDistrib,5)))))
  
  ## Expected value
    plot(x,ExpDensExpect(x),type="n", xlab="x-value",
      ylab=bquote(paste(x%.%f(x),"  at  ", lambda %==% .(lambda))))
    abline(v=0,lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
    EstExpect <- IntBoxes(ExpDensExpect,xMin,xMax,nBoxes)
    lines(x,ExpDensExpect(x),type="l",col="red",lwd=3)
    title(main=bquote(paste("Expectation: ",integral(x%.%f(x)%.%dx, .(xMin), .(xMax)) %~~% .(round(EstExpect,5)))))
    
  ## Variance
    plot(x,ExpDensVar(x),type="n",xlab="x-value",
         ylab=bquote(paste((x-1/lambda)^2%.%f(x),"  at  ", lambda %==% .(lambda))))
    abline(v=0,lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
    EstVar <- IntBoxes(ExpDensVar,xMin,xMax,nBoxes)
    lines(x,ExpDensVar(x),type="l",col="red",lwd=3)
    title(main=bquote(paste("Variance: ",integral((x-over(1,lambda))^2%.%f(x)%.%dx, .(xMin), .(xMax)) %~~% .(round(EstVar,5)))))
} ## End::Not Rieman Sum Tutorial
if (riemanTut){
## Systematice Diagram
  xMin <- 0
  xCut <- 6
  nBoxes <- 3
  x <- seq(xMin,xCut,length.out=600)
  
  plot(x,ExpDens(x),type="n",xlab="x-value",
       ylab=bquote(paste(f(x),"  at  ", lambda %==% .(lambda))))
  abline(v=c(0,2,4,6),lty="dotted",col="grey"); abline(h=0,lty="dotted",col="grey")
  ExpDistrib <- IntBoxes(ExpDens,xMin, xCut, nBoxes)
  title(main=bquote(paste("Rieman-Sum Approximation: ",integral(lambda%.%exp(-lambda%.%x)%.%dx, .(xMin), .(xCut)) %~~% .(round(ExpDistrib,5)))))
  
  xLow <- seq(0,1,length.out=100)
  polygon(c(xLow,0,0),c(ExpDens(xLow),ExpDens(1),ExpDens(0)),col="lightblue")
  text(0.35,0.5,"Under-\ncounted\nArea",col="blue4")
  xLow <- seq(1,2,length.out=100)
  polygon(c(xLow,2,1),c(ExpDens(xLow),ExpDens(1),ExpDens(1)),col="lightsalmon")  
  text(1.6,0.3,"Over-\ncounted\nArea",col="red4")
 
  lines(x,ExpDens(x),type="l",col="red",lwd=3)
  
  arrows(0,0.8,2,0.8,code=3); text(1,0.83,expression(dx[1]))
  arrows(1,0.4,1,0.45,code=1); text(1,0.48,expression(t[1]))
  arrows(2,0.5,4,0.5,code=3); text(3,0.53,expression(dx[2]))
  arrows(3,0.06,3,0.11,code=1); text(3,0.14,expression(t[2]))
  arrows(4,0.2,6,0.2,code=3); text(5,0.23,expression(dx[3]))
  arrows(5,0.02,5,0.07,code=1); text(5,0.1,expression(t[3]))
} ## End::Tutorial Plot 