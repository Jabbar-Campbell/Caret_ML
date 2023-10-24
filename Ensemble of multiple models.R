# Use the training set to build a model with several of the models available from the caret package. We will test out seven of the most common machine learning models 
# in this exercise:

# list of models we plan to use
# and pass into our caret::train()
models<- c("glm","lda","naive_bayes","knn","gamLoess","qda","rf")


library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)

data("mnist_27")


# Now we have a list of models
# we can loop thru them and compute  a table
# of y_hats and accuracies
fits<-lapply(models, function(model){
       print(model)
      train(y~ ., method = model, data = mnist_27$train)
  
})

# This is the accuracy of how well the model fits the data
# not the accuracy of the prediction which is below..
# filtering out models which don't grasp the data well dont need to be used
# in predicition
lapply(1:7, function(fidelity){
fits[[fidelity]]$results$Accuracy
}) %>% unlist %>% mean()

# looping thru each model and applying a prediction
# we have a table with the columns as the y_hat 
# for each model 1 thru 7
 y_hats<-sapply(fits, function(fit){
        predict(fit,mnist_27$test)
          })
dim(y_hats)
y_hats<-y_hats %>% as.data.frame()
colnames(y_hats) <- models  
 
 
   
Accuracies<- lapply(1:7, function(y_hat){
             confusionMatrix(factor(y_hats[,y_hat]),
                             mnist_27$test$y)$overall["Accuracy"]
  }) %>%
  unlist %>% 
  as.data.frame() %>% 
  summary()
  





# Here we count the number of times 7 is predicted accurately
# and express as a percentage of the total attempts
# those that predict a 7 more tha 50 % of the time  are 7 otherwise 2                

 
y_hats$no_sevens_per_row<- lapply(1:dim(y_hats)[1], function(x){
                    sum(y_hats[x,3:7] == "7")
                    }) %>% unlist()

y_hats<-y_hats %>% mutate(percent_7= no_sevens_per_row/7)


y_hats$combined_cutoff<-ifelse(y_hats$percent_7 >.5 ,7,2)

# This is another way to combine the models beside averaging the
# probability values from predict
confusionMatrix(factor(y_hats$combined_cutoff),mnist_27$test$y)




