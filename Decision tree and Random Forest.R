install.packages("tidyverse")
install.packages("rpart")
install.packages("dslabs")

library(tidyverse)
library(rpart)
library(dslabs)

polls_2008

plot(polls_2008)
###################################### DECISION TREE #############################

# Previous we ran linear and not linear regression models
#   We used caret and the models that it holds
#    no we use a new package called RPART for 
# RECURSIVE PARTIIONING AND REGRESSION TREES
#    In the  decision tree  a random subset 
#    of features are used  calculate a single tree

tree_fit<-rpart(margin~., data = polls_2008)
plot(tree_fit, margin =.1)
text(tree_fit,cex=.5)


# like any model we can use it
# to make predictions
# plot the data
# plot the predicted data in red
polls_2008 %>% 
  mutate(y_hat=predict(tree_fit)) %>% ##test independent data doesnt need to be supplied????
  ggplot()+
  geom_point(aes(day,margin))+
  geom_step(aes(day,y_hat),col = "red")


# This package can also be reached thru Caret
# as we seen before the parameters can be altered
# by supplying tunegrid with a dataframe
# the bootstrap can be altered with the trcontrol argument
install.packages("caret")
library(caret)

tree_fit2<-train(y ~ .,
           method = "rpart",
           tuneGrid = data.frame(cp= seq(0,1,len = 25)),
           data = mnist_27$train)


ggplot(tree_fit2)

# outside of caret rpart objects
# are visualize on there own
# inside caret theres a column called
# $finalmodel that is reference

plot(tree_fit2$finalModel, margin = .1)
text(tree_fit2$finalModel, cex = .5)

# of course predictions can be run 
# and compared to test data for acuraacy
# as with all caret models predict() knows what is dependent
# and what is independent
confusionMatrix(predict(tree_fit2,mnist_27$test), mnist_27$test$y)$overall["Accuracy"]



###########################################  RANDOM FOREST############################
# In the Random Forest the average of the
# above trees are combined into a single model
# you can control the tree size with the 
# nodesize and maxnodes arguements


forest_fit<-train( y  ~ .,
                   method = "Rborist",
                   tuneGrid = data.frame(predFixed = 2, minNode = c(3,50)),
                   data = mnist_27$train)

y_hat<-predict(forest_fit,mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]


###### Q1
# Create a data set where predictor
# increases .75 on average for every increase
# in a predictor

n<- 1000
sigma <-.25
set.seed(1)
x<-rnorm(n,0,1)
y<- .75 * x + rnorm(n,0,sigma) # gotta add some noise
dat<-data.frame(x=x,y=y)
dat


###### Q2
tree_fit<- rpart(y ~ ., data = dat)

plot(tree_fit)

###### Q3
dat %>% 
  mutate(y_hat= predict(tree_fit,dat)) %>% 
  ggplot()+
  geom_point(aes(x,y))+
  geom_step(aes(x,y_hat))

###### Q4
install.packages("randomForest")
library(randomForest)
forest_fit<- randomForest(y ~ ., data =dat)

dat %>% 
  mutate(y_hat= predict(forest_fit,dat)) %>% 
  ggplot()+
  geom_point(aes(x,y), alpha = .1)+
  geom_step(aes(x,y_hat))

###### Q5
plot(forest_fit)

###### Q6
