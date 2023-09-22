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
# People call this a Monte Carlo Simulation
qplot(M,bins =30, color = I("black"))



# Distribution allows us to calculate Confidence Interval
# in a single sample of 100 incomes X the confidence interval would be the folowing
median(X) +1.96 * sd(X)/ sqrt(N) * c(-1,1)


# M is more  many samples of 100 and would give us  closer readings
# on our actual confidence interval with
# you can run the above for each sample or...
quantile(M,c(.25,.95))


# even you took enough boot strapped data and boot strapped that
# the average metric would be more indicative then any single boot strap
# for a given metric







# we can then plot ???? 
# I don't understand this plot or the syntax whats x and whats y
# what are we plotting the medians against? why is it scaled?
qplot(sample = M, xlab = "theoretical", ylab = "sample")+
  geom_abline()
