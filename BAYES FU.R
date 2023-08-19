#####################BAYES THEOROM  P(A|B)  =  P(B|A)    *      (P(A) / P(B))

#         Since
#   Healthy + Disease =1
#            and
#### p(test - | Healthy) = .90
#### p(test + | Disease) = .85
#              then
####### p(test + | Healthy) = .10
####### p(test - | Disease) = .15




set.seed(1)
# 0 means the person is healthy   
# 1 means the person has the disease   
# setup a million random zeros and ones
disease <- sample(c(0,1), size=1e6,replace = TRUE, prob = c(.98,.02))



################################## ORIGINAL DATA with PREVALANCE #########################################
# set up a test with a million empty values
test<- rep(NA,1e6)

table(disease)




############################### TESTED DATA with % RELIABILITY 
# 0  means the test negative
# P(test - | healthy)  = .90
# set up zeros and ones amounting to the zeros indicating healthy folks  and assign to a 
# subst of test at the location of disease being 0 
# 90% will be 0 and 10 percent will be 1 (all add up to total ZEROS in orignial data) 
test[disease==0] <- sample(c(0,1), size = sum(disease == 0), replace = TRUE, prob = c(.90,.10))
 

# 1 means the test positive
# P(test + | disease)  = .85
# set up zeros and ones amounting to the zeros in the disease data and assign to a 
# subst of test at the location of disease being 1
# 15% will be 0 and 85 percent will be 1 (all add up to total ONES in orignial data)
test[disease==1] <- sample(c(0,1), size = sum(disease == 1), replace = TRUE, prob = c(.15,.85))


 #################################### THE MEAN OF a BOOLEAN IS PERCENTAGE #################################

# probability that you have the disease if the test is negative
# is a comparison of tested data to actual data
# find all the negative result in the tested pool set 
# this will include both healthy and diseased folks
# then index those in the original data.  
# Another way would be the subset that tested negative compared to the original data
mean(disease[test==0])


# probability that you have the disease if the test is positive
# is a comparison of tested data to actual data
# find all the positive results in the tested pool set 
# this will include both healthy and diseased folks
# then index those in the original data.   
# Another way would be the subset that tested positive compared to the original data
mean(disease[test==1])
mean(disease[test==1])


.149+.003
