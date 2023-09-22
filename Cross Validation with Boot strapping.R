# The idea will be to boot strap or sample this data and then get
# a feel for what the lowest MSE can be
# bootstrap 1 will have an MSE
#    bootstrap 2 will have an MSE
#       bootstrap 3 will have an MSE

# k=3 will have an average MSE
# the K that gives the lowest average MSE is our ideal setting


set.seed(1995)

n<-10^6


#set up a random normal distribution of numbers with standard deviation
income<-10^(rnorm(n,mean = log10(45000),sd =log10(3)))
qplot(log10(income),bins = 30, color = I("black"))



m<-median(income)

N<-100 # our sample size i think the closer to the total data the more accurate

# take N samples at random
# this differs from caret's createDataPartition() which gives indexs
# we can then run stats on this sampled data
X<-sample(income,N) 
median(X)


#instead of using a list like apply
# replicate performs a task n number of times
# Here we sample income 100 ways 10000 times over 
# and report the median
# each time giing us 10000 medians
library(gridExtra)
B<-10^4
M<-replicate(B,{
  X<-sample(income,N)
  median(X)
  
} )

# a histograms of these medians shows a normal distribution
qplot(M,bins =30, color = I("black"))

# we can then plot ???? 
# I dont understand this plot or the syntax whats x and whats y
# are we plo
qplot(sample = scale(M), xlab = "theoretical", ylab = "sample")+
  geom_abline()
