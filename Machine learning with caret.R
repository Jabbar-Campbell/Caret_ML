#! /bin/R

install.packages('dslabs')
install.packages("caret")
install.packages('heights')
library(dslabs)
library(tidyverse)
library(caret)


# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
# x_Train x_Train x_Train x_Train x_Train | y_hat
-------------------------------------------------
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat
# X_test  X_test  X_test  X_test  X_test  | y_hat


data(heights)


# Define the outcome and the predictors

x<- heights$height #what we want to use to drive out prediction
y<- heights$sex  #what we're trying to predict
length(y)

# Generate training and test data First we randomly sample 
# our data and giving and mark those positions with and index number
# this is done with the createDataPartition. Those indexs are assigned as Test data and everthing else 
# we set as  to training data
# its just a key really 
set.seed(2007)
test_index<-createDataPartition(y,times = 1, p = .05, list = TRUE)
x_Test<-heights[test_index,]# a subset index's
x_Train<-heights[-test_index,]# everything else


# sample the sex column taking the same number of samples as the index
y_hat<-sample(x = c("Male","Female"), size = length(test_index), replace = TRUE)



# we can compare these to methods of sampling and check for accuracy
# numbers of times the values is the same
mean(y_hat == x_Test$sex)


# We see on average the heights are close
# between sexs
heights %>% 
  group_by(sex) %>% 
  summarize(mean(height),sd(height) )




# without partitioning anything
# lets make a rule(model) that says anything greater
# than 62 is male and save the result as y_hat2
# turns out this is more accurate than our sampling method
y_hat2 <- ifelse(x > 62, "Male", "Female")
mean(y == y_hat2)






#set a list of cutoffs(rules) and pick which one
cutoff<-seq(61,70)
# like an lapply for every element in cutoff perform
# the function that says if the height of out training
# data is great then the cutoff value your male 
# well get a series of tables one for each cut off
# each is compared to the training data for accuracy

lapply(cutoff,function(x){
  y_hat3 <- ifelse(x_Train$height > x, "Male" , "Female")   
    #factor(levels = levels(x_Test$sex))
  mean(y_hat3 == x_Train$sex)
  
} )



# ACCURACY isn't enough since bias warps our predictions
# A table of predicted vs actual data gives us valuable info 
# we sampled  sex data based on indices and compared it to a sample(data) function on sex  
# of the predicted female 7 where female the rest where actually males
# of the male category 5 where female and 24 where actually males
# this shows that males are correctly estimated more than females are
table(predicted_data = y_hat, actual_data = x_Test$sex)






######################################### SENSITIVITY AND SPECIFICITY############################################## 
# 1 = positive
# 0 = negative

# SENSITIVITY(predicted, actual) 
# is the ability to predict a positive outcome
# when the actual outcome was also positive ie
# y_hat == 1 when y_test or y _train == 1
# its a ratio of the true positives 
# divided by the true positive plus False Negatives

# SPECIFICTY(predicted,actual) 
# if we can go the other way and predict negative cases
# as well then this speaks to specificity
# y_hat == 0 when y_test or y_train == 0

# CONFUSIONMATRIX(predicted,actual)
# a confusion matrix expresses accuracy, sensitivity, specificity and prevelance
# cm<-confusionMatrix(data = factor(y_hat), reference = factor(x_Test$sex))
# cm$overall["Accuracy"]
# cm$byClassic[c("Sensitivity","Specficity","Prevalence")]

# F_MEAS(predict,actual)
# summarized the confusion matrix in a single value

# Balanced accuracy is another way so summarize this in a single value
# called an F score. 
# Plotting this score against
# our cutoffs in y_hat3 we get a value of 66. 
# which is where our sensitive and specificity is best
lapply(cutoff,function(x){
  y_hat3 <- ifelse(x_Train$height > x, "Male" , "Female")   
  #factor(levels = levels(x_Test$sex))
  #mean(y_hat3 == x_Train$sex)
   TPR = sensitivity(y_hat3, x_Test$sex)
   TNR = specificity(y_hat3, x_Test$sex)
   FPR = 1-specificity(y_hat3, x_Test$sex)
   cm =   confusionMatrix(data = factor(y_hat3), reference = factor(x_Train$sex))
   F = F_meas(data = y_hat3, reference = factor(x_Train$sex) )
} )



# Another  tool is the ROC curve. This plot the specificity 
# against sensitivity



data("heights")
data(heights)
heights
class(heights)
str(heights)
dim(heights)
heights[777,1]
summary(heights)
which('sex' == "female" && heights$height > 78)
heights
dplyr::filter(heights, heights$height > 78)
library dplyr::where
 

ncol(df$train$images)

df<-read_mnist()
str(df)
60000*784


