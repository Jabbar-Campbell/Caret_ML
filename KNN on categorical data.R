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

dim(heights)
set.seed(1) 

# Sets the partition
index<-createDataPartition(heights$height, times = 100, p = .5)          
df<-as.data.frame(1:100)
colnames(df)<-"row"
df$indices<-index

heights[index[[2]],]


# Sets my train Data
Train<-heights[index$Resample1,]
tmp<-list()
for( i in 1:100){
  tmp[[i]]<-heights[index[[i]],]
  tmp
}
df$Train<-tmp
 

#Sets my Test data
Test<-heights[-index$Resample1,]
tmp<-list()
for( i in 1:100){
  tmp[[i]]<-heights[-index[[i]],]
  tmp
}
df$Test<-tmp






# knn with k = 3
knn3(sex ~ . ,data = Train, k=3)
knn_fits<-lapply(c(1,101,3),function(x)knn3(sex ~ ., data = Train, k = x))

# knn on a list of K's
#knn3(sex~., data = df$Train[[1]], k=1)
#df$knn_fits<-lapply(seq_along(df$Train),function(x)knn3( sex ~ ., data = df$Train[[x]]))

# predictions on knn
#predict(knn_fits,Test)# list of probabilities
#predict(knn_fits,Test, type = "class")# list of y_hat predictions
#predict(df$knn_fit_1[[1]],df$Test[[1]], type = "class")

# confusion matrix of knn y_hats (sex) vs orignal test column sex
# compare each to the test for accuracy
#confusionMatrix(as.factor(knn_hats[[1]]),as.factor(Test$sex))
#lapply(knn_hats,function(x)confusionMatrix(as.factor(x),Test$sex))
#confusionMatrix(as.factor(df$y_hats_1[[1]]),as.factor(df$Test[[1]]$sex))$overall[[1]]

#The measure "F" is a combination of precision and recall
# comute F_meas using reference[1] or Female as default 
#F_meas(as.factor(knn_hats[[3]]), Test$sex)
#lapply(knn_hats,function(x)F_meas(data = as.factor(x),reference = Test$sex,relevant = levels(Test$sex)[1]))




### we want to run knn for varying K's c(1,101,3) the way its done here is for each k we.....
# make a list of models based on the boot strap. 
#  make predictions using respective test data
#   compare those predictions to respective test data using a Confusion matrix for accuracy
#    compare those predictions to respective test data using a F_meas()  
#         outputs will be appended to a dataframe so we can keep track

############# for K = 1
for(i in 1:100){
  tmp[[i]]<-knn3(sex ~ . , data = df$Train[[i]], k =1)
  tmp
}
df$knn_fit_1<-tmp

for(i in 1:100){
  tmp[[i]]<-predict(df$knn_fit_1[[i]],df$Test[[i]], type = "class")
  tmp
}
df$y_hats_1<-tmp

for(i in 1:100){
  tmp[i]<-confusionMatrix(as.factor(df$y_hats_1[[i]]),as.factor(df$Test[[i]]$sex))$overall[[1]]
  tmp
}
df$accuracy_1<-tmp

for(i in 1:100){
  tmp[i]<-F_meas(as.factor(df$y_hats_1[[i]]), df$Test[[i]]$sex) 
  tmp
}
df$f_meas_1<-tmp
#################



############# for K = 101
for(i in 1:100){
  tmp[[i]]<-knn3(sex ~ . , data = df$Train[[i]], k =101)
  tmp
}
df$knn_fit_101<-tmp

for(i in 1:100){
  tmp[[i]]<-predict(df$knn_fit_101[[i]],df$Test[[i]], type = "class")
  tmp
}
df$y_hats_101<-tmp

for(i in 1:100){
  tmp[i]<-confusionMatrix(as.factor(df$y_hats_101[[i]]),as.factor(df$Test[[i]]$sex))$overall[[1]]
  tmp
}
df$accuracy_101<-tmp

for(i in 1:100){
  tmp[i]<-F_meas(as.factor(df$y_hats_101[[i]]), df$Test[[i]]$sex) 
  tmp
}
df$f_meas_101<-tmp
#################





#############  for K = 3
for(i in 1:100){
  tmp[[i]]<-knn3(sex ~ . , data = df$Train[[i]], k =3)
  tmp
}
df$knn_fit_3<-tmp

for(i in 1:100){
  tmp[[i]]<-predict(df$knn_fit_3[[i]],df$Test[[i]], type = "class")
  tmp
}
df$y_hats_3<-tmp

for(i in 1:100){
  tmp[i]<-confusionMatrix(as.factor(df$y_hats_3[[i]]),as.factor(df$Test[[i]]$sex))$overall[[1]]
  tmp
}
df$accuracy_3<-tmp

for(i in 1:100){
  tmp[i]<-F_meas(as.factor(df$y_hats_3[[i]]), df$Test[[i]]$sex) 
  tmp
}
df$f_meas_3<-tmp
################# 




df %>% mutate(., avg_f_meas1 = mean(unlist(f_meas_1)))

# for each f measure we can see which has the highest score 
df$max_f_meas1<- max(unlist(df$f_meas_1))
df$max_f_meas101<- max(unlist(df$f_meas_101))
df$max_f_meas3<- max(unlist(df$f_meas_3))

unique(df[,c(17,18,19)])
unique(df[,c(20,21,22)])

which(unlist(df$f_meas_101)>.644)



unlist(df$f_meas_101)[which(unlist(df$f_meas_101)>.6)] %>% min()

unlist(df$f_meas_1)[which(unlist(df$f_meas_1)>.6)] %>% min()

unlist(df$f_meas_3)[which(unlist(df$f_meas_3)>.6)] %>% min()

str(df[,c(8,12,16)])


max()