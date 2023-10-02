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



########################################HOMEWORK############
##########################################Q1 Q2################

# Going back to our 2 vs 7 data problem
# lets resample the entire data set
# unlike creating a partition sampling allows us
# to pick the same index more than once
# caret::createResample()returns indices unlike stat::sample()
library(dslabs)
library(caret)

data("mnist_27")
set.seed(1995)


indexs<- createResample(mnist_27$train$y,10)

# how many times does 7 occur in the first data frame
which(indexs[[1]]==7) %>% as.data.frame() %>% count

#how many times does 3 occur across all dataframes
lapply(indexs,function(x)
              no_3<-(which(x == 3))) %>% unlist %>%data.frame %>% count
 
 
                                      #### Q 3 #####
# 75 quantile of the data 1 being the 100th quantile
qnorm(.75)


# on the y space where would 75 percent be is given by
quantile(y,0.75)



############################### MONTE CARLO #####################################
# takes samples at random and without duplication
# when sample size is the same as the data
# its like a reshuffling which I feel
# is the same as the orginal data just in different orders
# isn't this better????

set.seed(1)
y<-rnorm(100,0,1)
B<-10^4
M_star<-replicate(B,{
  X_star<-sample(y,length(y),replace = TRUE) # this is a sample
  quantile(X_star,.75) # theoretical quantile for each reshuffle
})

# average of our generated quantiles
mean(M_star)
# vs original quantile
quantile(y,.75)
sd(M_star)/sqrt(length(M_star))


  



# ################################# BOOT STRAPPING ######################
# Not only randomly samples the original data but also selects indices of the sampled data 
# (kind of like sampling from the sample)
# computationally this is better than resampling a gazillion times.... 

set.seed(1)
y<-rnorm(100,0,1)
indexs<-createResample(y,10000) 

indexs[[1]]
z<-y[indexs[[1]]]
quantile(z,0.75)


# from these index's obtain the values for each sample
# along with the 75 the quantile as per example above
# the data set will have changed because some samples
# will be counted more than once
mc=list()
tmp_75=list()
btstr=list()
btstr_75=list()

for (i in 1:10000){
  X_star<-sample(y,length(y),replace = TRUE)# this is the monte carlo of original data
  #mc[[i]]<-y[c(indexs[[i]])] 
  #mc_75[[i]]<-quantile(tmp[[i]],0.75) #75th quantiles of each
              index<-createResample(X_star,1) #indices of sampled data
              btsr[[i]]<-X_star[c(indexs[[i]])] # this is the boot strap or or values of sampled data
             #btstr[[i]]<- sample(mc_75)   # this is the bootstrapped step selecting a few histograms
             btstr_75[[i]]<-quantile(btstr[[i]],0.75) # 75th quantile of the boot strapped data
  tmp
  
}

# average of our generated quantiles
unlist(Q_75) %>% mean()
# vs orignal data
quantile(y,.75)


sd(unlist(Q_75))/sqrt(length(unlist(Q_75)))





# we can then plot ???? 
# I don't understand this plot or the syntax whats x and whats y
# what are we plotting the medians against? why is it scaled?
qplot(sample = M, xlab = "theoretical", ylab = "sample")+
  geom_abline()
