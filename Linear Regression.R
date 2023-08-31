# x_Train x_Train | y_train y_train
# x_Train x_Train | y_train y_train
# x_Train x_Train | y_train y_train
# x_Train x_Train | y_train y_train
# x_Train x_Train | y_train y_train
----------------------------------
# X_test  X_test  | y_test y_test
# X_test  X_test  | y_test y_test

library(purrr)
library(caret)
library('ggplot2')


# Sometimes we have more than one thing to predict ie y_hat1 and y_hat2
# and 2 or more  features ie x1 and x2 
# p(x1,x2) = p(Y = 1 | X1 X2)  

# a linear model of this would be 
# Bo + B1*x1 + B2*x2
# in R we have 
# my_model<-lm(y~ x1 + x2, data = x_Train x_Train | y_train y_train )

# you can run predictions on it and assign 
# x_hat<- predict(my_model, newdata = x_Train) # now we have simulated x_data not actual x data
# y_hat <- ifelse( x_hat > .5, yes,no)
# confusionMatrix(y_hat, y_train )


 
RMSE_N = function(n){
  
#n<-100
# First we create our data set of a certain size 
Sigma<-9*matrix(c(1,.5,.5,1),2,2) # a 2x2 matrix for covariance
DAT<-MASS::mvrnorm(n = n, c(69,69),Sigma) %>% #is this 3d space
  data.frame() %>% # I think we turned 3d into 2d
  setNames(c("x","y"))
  



# we will make 100 linear models partitioning the data 100 different
# ways and generating a linear model from training data on each. 
# there will be predictions using test data and our our linear model
# and  Root Mean Square Error,  and SD will be reported 


# we have will sampled the data according to indices 100 different ways
# using predictor or output shouldn't matter?????????????
y=DAT$x
index<-createDataPartition(y,times = 100, p = .5, list = TRUE)


dat<-data.frame(line = 1:100)
#Train set are a filtered subset of original data according to index
output<-list()
for (i in 1:100){
  output[[i]]<-DAT[index[[i]],]
  output[[i]]<-as.data.frame(output[[i]])
  output[i]
}

#assign to a column
dat$x_train <- output
dat$indices<-index


#Test set are NOT the index's
#dat[,c(1,2)][-c(dat$indices[[1]]),]
output<-list()
for (i in 1:100){
  output[[i]]<-DAT[,c(1,2)][-c(dat$indices[[i]]),]
  output[[i]]<-as.data.frame(output[[i]])
  output[i]
}
#assign to a column
dat$x_test <- output

  




#I need to fit lm for each row of the x_train column
Model<-lapply(seq_along(dat$x_train), function(x)lm(y ~ x + y, data = dat$x_train[[x]] ) )
#assign to a column
dat$Model<-Model



# run predictions for the X variable of  x_test[,]  using each respective linear model(from the training data) 
# to give us  a y_hat
dat$x_test
lapply(dat$x_test,`[`,"x")[1]
predict(dat$Model[[1]],lapply(dat$x_train,`[`,"x")[[1]])

test<-mapply(predict, dat$Model, lapply(dat$x_test,`[`,"x"))

test<-t(test)
test<-as.data.frame(test)
test$id<- 1:100
test<-test %>% dplyr::nest_by(id) %>% as.data.frame()


#assign to a column 
dat$y_hats<-as.list(test$data)
dat$y_hats<-lapply(dat$y_hats, as.data.frame)
dat$y_hats<-lapply(dat$y_hats, as.numeric)






## we make dataframes of the  X variable from  x_test[,] data 
# and predicted y_hat data for ggplot to use so we can draw the model in red
#pred_df<-data.frame(x=dat$x_train[[1]]$x,y=dat$y_hats[[1]]) 

pred_df<-list()
for (i in 1:100){
  pred_df[[i]] <-data.frame(x=dat$x_test[[i]]$x,y=dat$y_hats[[i]]) 
  pred_df
}
#assign to a column
dat$pred_df<-pred_df




# we make plots 
# gray dots is our original data and a line which is our prediction using a 
# model we made from training data but using  "Test" and predicted Y
# we can compare our linear model to any of the sets
dat$x_train[[i]]
dat$x_test[[i]]
dat[,c(1,2)]

plot<-list()
for (i in 1:100){
  plot[[i]]<-ggplot(data = dat[,c(1,2)],aes(x=x, y=y))+
           geom_point(aes(size =2, alpha=.5))+
           geom_line(color = "red",size =1.5,alpha = .5,data = dat$pred_df[[i]], aes(x=x,y=y))+
           theme_bw()+ labs(title = paste0( "Plot no ",i ))
  plot
  
}
# assign it as a column
dat$plots<-plot

plot[100]


# lets also add RMSE using a function where m is for model (fitted) values, o is for observed (true) values.
# we compare our original y data to our predicted y data which in this case forms a line

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

rmse<-list()
for (i in 1:100) {
      rmse[i]<- RMSE(dat$y_hats[[i]],dat$x_test[[i]]$'y')  
      rmse
  
}

#assign to a a column
dat$rmse<-rmse


mean(unlist(dat$rmse))
print (  paste("RMSE of", n, "is", mean(unlist(dat$rmse))) )

sd(unlist(dat$rmse))
print (paste("SD of", n, "is", sd(unlist(dat$rmse)))  )
}

set.seed(1)
#set.seed(NULL)
#RMSE_N(100)
sapply(c(100,500,1000,5000,10000), RMSE_N)
 