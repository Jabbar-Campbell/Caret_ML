############################ REGULARIZATION ###################################
### is all bout looking at deviation from the mean and see which class is more prone
# once we realize this we can assign a penality.
# we can try a series of penalties to see which parameter give the most accurate penality
# to a given feature.
RMSE<-function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))


set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))


schools %>% top_n(10, quality) %>% arrange(desc(quality))


set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))




schools %>% top_n(-10, score) %>% arrange(desc(size))

schools %>% top_n(10, score) %>% arrange(desc(score))

schools %>% mutate(.,SD= sd(score) ,BI = score- mean(score)  ,SE =  sd(score)/ sqrt(nrow(schools)))

#schools %>% top_n(-10, score) %>% mutate(., se= stderr(score))
 

#Let's use regularization to pick the best schools. Remember regularization shrinks deviations from the 
#average towards 0. To apply regularization here, we first need to define the overall average for all schools, 
#using the following code:
      
    
# Then, we need to define, for each school, how it deviates from that average.
# Write code that estimates the score above the average for each school but dividing by  n + alpha 
# instead of the number use  n = school size and  a regularization parameter. 
#Try  alpha = 25.
x<-schools %>% group_by(.,size) %>% mutate(., no_per_size = table(size)) #%>% merge(.,schools)


# no per size is a count of size
# we add to  the mean score a weighted function that scales itself by the prevelance of the size/class. Those that 
# repeat themselves or occur more times get  a larger adjustment(penality) to the average score. 
# if the size of the school is under represents (no_per_size is small) the penalty is less
x<-merge(schools,x)
overall <- mean(sapply(scores, mean))
x$mu<-overall

RMSE<-function()
  
#I'm trying to loop thru parameters calculating the RMSE between a weighted prediction and the actual scores as a list
# then plotting this list of RMSE's  to see which Parameter gives the lowest RMSE
#   

#  The regularized score (your y_hat) should be calculated in the same way as Q5.
#  The quality variable should only be used in the RMSE calculation.  
#  You are removing the overall average (-overall) when you should not be as instructed in the Q8 prompt.
library(tidyverse)
  
  Alpha<-seq(10,250,1)

mutate(x, y_hat = mu +( score / (no_per_size + 25)) ) %>% top_n(.,10)  %>% arrange(.,desc(y_hat)) 
sum(114-overall)
 
my_RMSE<-lapply(Alpha, function(i){ 
score_reg <- sapply(scores, function(x)  
  overall /(length(x)+i)) ###here we tack on the SEM or standard error of the mean
                                            # its basically the Standard deviation of the mean
z<-schools %>% mutate(score_reg = score_reg)  
 RMSE(z$quality,z$score_reg)
}) %>% unlist()


#with out removing overall mean????
alpha <- 131
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


data.frame(Alpha,my_RMSE) %>%  arrange(.,desc(my_RMSE)) %>% 
  plot()


x<-x %>% mutate(., y_hat =mu+( score / (size + 25)) ) %>% top_n(.,10,y_hat) %>% arrange(.,desc(y_hat))



x %>% mutate()


77/(213+25)
