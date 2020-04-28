rm(list=ls())
## Data vector with ties in the last four elements
x <- c(4.0,4.4,3.8,2.5,5.1,4.5,3.8,4.8,4.4,4.1)

## Sorting
( xSort <- sort(x) )                          # works only on vectors

## Re-ordering works on matrices and data-frames
shuffle <- order(x)                            # Order generates a shuffle index
xOrdered <- x[shuffle]                           # Shuffle data positions in vector
(cbind(x, shuffle, xOrdered))

## Ranking data
(xRank <- rank(x, ties.method="random"))      # Explore other methods

## quantiles Q[i](p) = (1 - z)*x[j] + z*x[j+1] with 0 <= z <= 1
( quantile(xOrdered,prob=seq(0.1,0.9,by=0.1)) ) 
( quantile(xOrdered,prob=c(0.25,0.5,0.75)) )    # Quartiles

## Percentage (probability) points - for tied data use the larger percentile
xPercent <- cbind("X-value"=xOrdered, "Percentile a=0.0"=ppoints(xOrdered,a=0.0),
                  "Percentile a=0.5"=ppoints(xOrdered,a=0.5), "Percentile a=1.0"=ppoints(xOrdered,a=1.0))
round(xPercent, 2)

## Empirical Distribution Function
summary(ecdf(x))
plot(ecdf(x))
car::qqPlot(x)
