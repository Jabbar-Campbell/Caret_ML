library(dslabs)
# this comes divided into train and test
mnist<-read_mnist()

names(mnist)
mnist$train
# both test and train have an images and labels or what number each image is
dim(mnist$train$images) ### is each an image i;m not sure
dim(mnist$test$images)

dim(mnist$train$labels)

# basically how much of each number
table(mnist$train$labels)

# to speed things up we can sample both training and test set
#
set.seed(1990)
indices<-sample(nrow(mnist$train$images),10000)

x<-mnist$train$images[indices,]
y<- factor(mnist$train$labels[indices])


indices<-sample(nrow(mnist$test$images),1000)
x_test<-mnist$test$images[indices,]
y_test<-mnist$test$labels[indices]


# we can see the SD of each column and see that some dont change
# much. 
install.packages("matrixStats")
library(matrixStats)
sds<-colSds(x)
qplot(sds,bin=256,color = I("Black"))


library(caret)
# Diagnose predictors that have mostly 
# the same value or are mostly the same value
# then we set up a matric that is within this
# region of  low variablity and image it
# I dont understand the 28 part????
nzv<-nearZeroVar(x) %>%  near
image(matrix(1:784 ))
image(matrix(1:784 ,28,28))

image(matrix(1:784 %in% nzv,30,30))
image(matrix(1:784 %in% nzv,28,28))

#since nzv identifies column with little variance
# we index and focus on columns with a decent range of vales
col_index<-setdiff(1:ncol(x),nzv)
length(col_index)


# In order to use KNN the columns of the data must have a label
colnames(x)<-1:ncol(mnist$train$images)
colnames(x_test)<-1:ncol(mnist$train$images)

# when we train we focus on the columns with 
# a decent range of values
# NOTE: the data  the  label( a list) and image (a dataframe) 
# are separate objects so we cant use the syntax of y ~ x

train(x[,col_index], y,
      method = "knn",
      tuneGrid = data.frame(k=c(3,5,7)),
      trControl = trainControl(method = "cv",
                               number = 10, 
                               p=.9))


# Since this takes forever we can take a sample 
# of the training data (which itself is a sample 
# to train our model
index<-sample(nrow(x),1000)
knn_fit<-train(x[index,col_index], y[index],
         method = "knn",
         tuneGrid = data.frame(k=c(3,5,7)),
         trControl = trainControl(method = "cv",
                               number = 10, 
                               p=.9))

predict(knn_fit, x_test[,col_index], type = "raw") %>% 
  confusionMatrix(.,factor(y_test)) #y_test used the same sample size

# When running a random forest since this is many tree
# over training can be an issue along with computation time
# to get around this we reduce the boot strap to 10 %
# as well as sample the training data

rf_fit<-train(x[index,col_index], y[index],
              method = "rf",
              nTree = 150,
              trControl = trainControl(method = "cv",
                                       number = 5),
              tuneGrid = data.frame(mtry = c(1,5,10,25,50,100)),
              nSamp = 5000)

predict(rf_fit, x_test[,col_index]) %>% 
  confusionMatrix(.,y_test)

# Random Forest or a mixtures of so many tree
# so there's no real way to visualize them in Caret 
# Random forest also has some commands that allow us
# to see which features are used the most often as the
# trees are being averaged with importance

imp<-randomForest(x[index,col_index], y[index]) %>% 
     importance(.)

mat<-rep(0,ncol(x))#make a blank space of numbers
mat[col_index]<-imp# subtitute a subset with the most imporant
image(matrix(mat,28,28)) # ??????again I dont get how the 28 comes in play???????


# we can do actual predicted values from our model
# as well as the probability that exist for each number add up to 1
y_hat<-predict(rf_fit, x_test[,col_index])

p_max<-predict(rf_fit, x_test[,col_index], type = "prob")
p_max<- p_max/rowSums(p_max) 

p_max<-apply(p_max, 1,max)#????

#give us the mismatches between predicted and actual values
ind<-which(y_hat != y_test)


# We can also combine the probabilities from 2 different models
# to yield a more comprehensive ENSEMBLE model 
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)#?????
confusionMatrix(y_pred, y_test)$overall["Accuracy"]


# mis matching postions are ordered
#according to the probablities in decreasing value?
ind<-ind[order(p_max[ind], decreasing = TRUE)]

# this is a package that the teacher invented
# 
install.packages("rafalib")
library(rafalib)
rafalib::mypar(1,4)
for(i in ind[1:4]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}