##
## Impact of Sample Size on Chi-Square Test
##
lowCount <- matrix(c(6,8,7,10),nrow=2)                   # Enter table
lowCount                                                 # Low counts
(low.test <- chisq.test(lowCount, correct=F))            # Notice warning
low.test$expected                                        # Low expected counts
chisq.test(lowCount, simulate.p.value=T)                 # Get p-value through simulation

##
## See what happens to chi-square value when rescaling the table
##
hiCount <- lowCount*1000
hiCount
(hi.test <- chisq.test(hiCount, correct=F))              # chi-square increased by 1000
hi.test$expected

