library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27
str(mnist_27)


library(caret)
train_glm<-train(y~., method= "glm", data = mnist_27$train)
train_knn<-train(y~., method = "knn", data = mnist_27$train)

y_hat_glm<-predict.train(train_glm,mnist_27$test,type = "raw")
y_hat_knn<-predict.train(train_knn,mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm,mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn,mnist_27$test$y)$overall[["Accuracy"]]


getModelInfo("knn")
modelLookup("knn")

ggplot(train_knn,highlight = TRUE)




# if we'd like to change the default for neighbors
# to a new sequence
tune<-data.frame(k=seq(9,67,2))
set.seed(2008)
train_knn<-train(y~., method = "knn",
                 data = mnist_27$train,
                 tuneGrid =  tune )

confusionMatrix(predict.train(train_knn,mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall[["Accuracy"]]

# here we reset the bootstrap that knn typicaly uses
# and then sets that thru trControl
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv<- train(y~.,method = "knn",
                     data = mnist_27$train,
                     tuneGrid = tune,
                     trControl = control)

ggplot(train_knn_cv, highlight = TRUE)

names(train_knn_cv$results)

plot_cond_prob <- function(p_hat=NULL){
  tmp<-mnist_27$true_p
  if(!is.null(p_hat)){
    tmp<-mutate(tmp, p = p_hat)
  }
  tmp %>% ggplot(aes(x_1,x_2,z=p,fill = p))+
    geom_raster(show.legend = FALSE)+
    scale_fill_gradientn(colors = c("#F8766D","white","#00BFC4"))+
    stat_contour(breaks = c(.5),color = "black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])


##################################################

https://topepo.github.io/caret/available-models.html

#####################################################
# CARET has many models
# they can me tuned by
# 1) inspect with modelLookup()
# 2) creating a table with the column names as the parameter
#   and a list of values ie data.frame(param_1 = seq(), parm_2 = seq())
# 3) substitite the parameters with the tuneGrid argument in train()
# 4) default bootstrap is 25 % but this can be changed using trainControl()
#    to set the trControl argumnet of train()


############################################## Q1 ################################
##################################################################################




library(tidyverse)
library(caret)

set.seed(1996)
n<-1000
p<-10000

x<- matrix(rnorm(n*p),n,p)
colnames(x)<- paste("x", 1:ncol(x), sep = "_")
y<-rbinom(n,1,.5) %>% factor()

x_subset <- x[,sample(p,100)] ## random columns selected


# since X and Y are made indepdent of one shouldnt be able
# to predict the other with anything greater than 50%
# notice instead of  y ~ . you can make a model from separate data
train(x_subset,y method = "glm" ) 

# Now, instead of using a random selection of predictors, 
# we are going to search for those that are most predictive 
# of the outcome. We can do this by comparing the values for 
# the  group to those in the  group, for each predictor, using 
# a t-test. We can perform this step like this:
# loop thru each predictor and t test 0 
 pvals<-rep(0,ncol(x))
 
 for (i in 1:ncol(x)) {
   pvals[i]<- t.test(x[,i][y==0],
                     x[,i][y==1],var.equal =TRUE)$p.value
   
 }

 ifelse(pvals > .01, "yes","no")
 
 which( ifelse(pvals < .01, "yes","no") == "yes") %>% length()
 
# Now set the seed to 1 and re-run the cross-validation after
# redefining x_subset to be the subset of x defined by the 
# columns showing "statistically significant" association with y.
 set.seed(1)
sign_col<- which( ifelse(pvals < .01, "yes","no") == "yes")
 
 x_subset<- x[,sign_col]
 
 train(x_subset,y,method = "glm")
 
 
 # ATTENTION
 # This is not the way to go because we are picking the columns
 # with the highest t.test score. This is a kind of bias
 
 
 set.seed(1)
 fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
 ggplot(fit)