# K Nearest Neighbors

library(tidyverse)
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)


data("mnist_27")

mnist_27$test %>% 
  ggplot(aes(x_1,x_2, color =y))+
  geom_point()

#Previously we learned it convert categorical data to binary
# in order to express probablity
# we fit a linear model and make predicition in the probablitiy space
# setting a cutoff we can compare predicted data to test data
fit_lm <- mnist_27$train %>% 
  mutate(y == ifelse(y == 7,1,0)) %>% 
  lm( y~ x_1 + x_2, data = .)

p_hat_lm<- predict(fit_lm, mnist_27$test)
y_hat_lm<- factor(ifelse(p_hat_lm > .5,7,2))
confusionMatrix(y_hat_lm,mnist_27$test$y)






# similar to lm and loess we can predict using KNN 
# but we dont even need to convert  2,7 into binary

knn_fit<-knn3(y~., data = mnist_27$train, type = "class")


# the KNN model predicts data in two ways......
#    1) The probablity of 2 or 7
#    2) y_hat given the test data 
predict(knn_fit, mnist_27$test) 
y_hat_knn<- predict(knn_fit, mnist_27$test, type = "class") 


#compare predicted with actual data
confusionMatrix(y_hat_knn,mnist_27$test$y)





#  we can make a function that visualizes PROBABLITIY
#  as a gradient....
plot_cond_prob <- function(p_hat=NULL){
  tmp <-mnist_27$true_p                  #make a data frame of x1 x2 and p
  if(!is.null(p_hat)){                   #if input has no nulls make that p column
    tmp<-mutate(tmp,p = p_hat)
  }                                      #plot x1 x2 and new p values 
  tmp %>% ggplot(aes(x_1,x_2, z=p,fill=p))+
    geom_raster(show.legend = FALSE)+
    scale_fill_gradientn(colors = c("#F8766D","white","#00BFC4"))+
    stat_contour(breaks = c(0.5),color = "black")
}


# p1 is the default and actual probability
# p2 uses are model probabilities

p1<-plot_cond_prob()+
    ggtitle("True Conditional Probability")


p2<-plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2])+
    ggtitle("KNN-5 estimate")
    
    
################################HOMEWORK########################
###############################################################]

### we want to run knn for varying K's seq(1,101,3) the way its done here is for each k we.....
# make a list of models based on the boot strap. 
#  make predictions using respective test data
#   compare those predictions to respective test data using a Confusion matrix for accuracy
#    compare those predictions to respective test data using a F_meas()  
#         outputs will be appended to a dataframe so we can keep track


###apply functions are easier the for loops just list the variable you want to use in th
# the function in this case elements of the list are the k variable.

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]



##########################################HOMEWORK QUESTION 2 #################################
####################KNN to predict tissue from gene expression#######################
data("tissue_gene_expression")
str(tissue_gene_expression)

set.seed(1)
class(tissue_gene_expression)

df<-as.data.frame(tissue_gene_expression)  
df$row<-1:189
index<-createDataPartition(df$y,times = 1, p = .5)


`%notin%` <- Negate(`%in%`)

Train<-filter(df, df$row %in% index[[1]])
Test<-filter(df, df$row %notin% index[[1]])


knn_tissue_fit<-knn3(y ~.,data = Train, k = 1)
y_hat<-predict(knn_tissue_fit, Test, type = "class")
confusionMatrix(as.factor(y_hat), as.factor(Test$y))$overall[1]

knn_tissue_fit<-knn3(y ~.,data = Train, k = 3)
y_hat<-predict(knn_tissue_fit, Test, type = "class")
confusionMatrix(as.factor(y_hat), as.factor(Test$y))$overall[1]

knn_tissue_fit<-knn3(y ~.,data = Train, k = 5)
y_hat<-predict(knn_tissue_fit, Test, type = "class")
confusionMatrix(as.factor(y_hat), as.factor(Test$y))$overall[1]
 

knn_tissue_fit<-knn3(y ~.,data = Train, k = 7)
y_hat<-predict(knn_tissue_fit, Test, type = "class")
confusionMatrix(as.factor(y_hat), as.factor(Test$y))$overall[1]

knn_tissue_fit<-knn3(y ~.,data = Train, k = 9)
y_hat<-predict(knn_tissue_fit, Test, type = "class")
confusionMatrix(as.factor(y_hat), as.factor(Test$y))$overall[1]


knn_tissue_fit<-knn3(y ~.,data = Train, k = 11)
y_hat<-predict(knn_tissue_fit, Test, type = "class")
confusionMatrix(as.factor(y_hat), as.factor(Test$y))$overall[1]




#####a faster way would be using map_df ...for each k the following
# will be run and stored in a data frame
ks<-c(1,3,5,7,9) # for k values
ks<-seq(3,251,2) # sequence of k values
map_df(ks,function(x){
  fit<- knn3(y ~ ., data = Train, k=k)
  y_hat_train<-predict(fit, Train, type = "class")
  acc_train<- confusionMatrix(y_hat, Train$sex)$overall[[1]]
  df_train<-as.data.frame(y_hat_train,acc_train)
  
  
  fit<- knn3(y~., data = Train, k=k)
  y_hat_test<-predict(fit, Test, type = "class")
  acc_test<- confusionMatrix(y_hat, Test$sex)$overall[[1]]
  df_test<-as.data.frame(y_hat_test,acc_test)
  
  tibble(Train = acc_train, test = acc_test)
})

# the table can be used in a ggplot to compare
# accuraces on the x axis and k value on the y axis
# coloring by Data type
accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)