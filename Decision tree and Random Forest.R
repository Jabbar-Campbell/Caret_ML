<<<<<<< HEAD
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
#    In the  decision tree  a random subset of features are used  calculate a single tree
#    see also rpart.control() for  parameters
#     cp is the complexity parameter. overtrain vs undertrain
#     minsplit is number of observations that must exist in a node and can affect accuracy

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
           data = mnist_27$train,
           control = rpart.control(minsplit = 20)))


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

###### Q 1.1
# Load the rpart package and then use the caret::train() function with method = "rpart" 
# to fit a classification tree to the tissue_gene_expression dataset. Try out cp values of seq(0, 0.1, 0.01).
# Plot the accuracies to report the results of the best model. Set the seed to 1991.

# Which value of cp gives the highest accuracy?
tissue_gene_expression$y
df<-tissue_gene_expression %>% as.data.frame(col.names = NULL)
df$tissue<-row.names(df)
rownames(df)<- c(1:dim(df)[1])
df<-df %>% 
    tidyr::separate(., col = tissue, into= c( "y", "tissue_no"), sep = "_" )
df$structure.c.1L..1L..1L..1L..1L..1L..1L..1L..1L..1L..1L..1L..1L..= NULL

tree_fit<-train( y~ .,
       method = "rpart",
       data = df[,-502],
       tuneGrid = data.frame(cp = seq(0,.1,.01)))

ggplot(tree_fit, max = "highlight")


# Note that there are only 6 placentas in the dataset. By default, rpart requires 
# 20 observations before splitting a node. That means that it is difficult to have a node in which placentas are the majority. 
# Rerun the analysis you did in Q1 with caret::train(), but this time with method = "rpart" and 
# allow it to split any node by using the argument control = rpart.control(minsplit = 0). 
# Look at the confusion matrix again to determine whether the accuracy increases. Again, set the seed to 1991.
set.seed(1991)
tree_fit<-train( y~ .,
                 method = "rpart",
                 data = df[,-502],
                 tuneGrid = data.frame(cp = seq(0,.1,.01)),
                 control = rpart.control(minsplit = 0))

ggplot(tree_fit, max = "highlight")

#  this model has a max accuracy of .903
#  we can see its complexity and only requires seven genes with the 
# following cuttoffs
plot(tree_fit$finalModel, margin = .1)
text(tree_fit$finalModel, cex = .5)

# We can see that with just seven genes, we are able to predict the tissue type. Now let's see if we can 
# predict the tissue type with even fewer genes using a Random Forest. Use the train() function and the rf 
# method to train a Random Forest model and save it to an object called fit . Try out values of mtry ranging 
# from seq(50, 200, 25) (you can also explore other values on your own). What mtry value maximizes accuracy? 
# To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1 .

=======
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
>>>>>>> 6283db36da4fee3cb982be59e5da71c749998b4c
