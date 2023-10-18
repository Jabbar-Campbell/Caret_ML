# The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 
# from the United Kingdom to New York. More than 1,500 of the estimated 2,224 passengers and crew died 
# in the accident, making this one of the largest maritime disasters ever outside of war. The ship carried 
# a wide range of passengers of all ages and both genders, from luxury travelers in first-class to immigrants 
# in the lower classes. However, not all passengers were equally likely to survive the accident. You will use real 
# data about a selection of 891 passengers to predict which passengers survived
# Use the titanic_train data frame from the titanic library as the starting point for this project
install.packages("titanic")
library(titanic)
library(caret)
library(tidyverse)
library(rpart)
##################################### HINT the table function allows for summary and percentage values #####################
################################################## based on how the data is grouped ########################################
##############################################MERGING THIS DATA BACK IN ALLOWS FOR CUTOFFS #####################################
options(digits = 3)

titanic_train %>% as.data.frame()
df<-titanic_train

titanic_clean<-titanic_train %>% 
               mutate(Survived = factor(Survived),
               Embarked = factor(Embarked),
               Age = ifelse(is.na(Age),median(Age,na.rm = TRUE),Age),
               FamilySize = SibSp + Parch + 1) %>% 
               select(Survived, Sex, Pclass, Age,Fare, SibSp, Parch, FamilySize, Embarked)


set.seed(42)
indices<-createDataPartition(titanic_clean$Survived, p =.20)# p is how much is reserved for test

test_set<-titanic_clean[indices$Resample1,]
train_set<-titanic_clean[-indices$Resample1,]

summary(train_set$Survived)

69/(110+69)


# The simplest prediction method is randomly guessing the outcome without using additional predictors. 
# These methods will help us determine whether our machine learning algorithm performs better than chance. 
# How accurate are two methods of guessing Titanic passenger survival?

########################################### y_hat at RANDOM ##########################################
# This flips a coin 179 times 
set.seed(3)
y_hat<-sample(c(0,1),replace = TRUE,size=179) %>% as.factor()
confusionMatrix(y_hat,test_set$Survived)


########################################### y_hat form  a PROBABLITY cut off #######################
# filtering on sex we can get the percentage of survival
# when compared to the whole this in essence is PROBABILITY !!!!!!!!!!!!!!!!!!

summary(train_set %>% filter(.,Sex == "female"))
Females-survived<- 184
females-died <- 67
summary(train_set %>% filter(.,Sex == "male"))
males -survived<-  89
males - died<-  372 

# we can assign probabilities for each sex
# but below a linear model also captures this
ifelse(test_set$Sex == "female", .733, .193)

# Predict survival using sex on the test set: if the survival rate for a sex is over 0.5, 
# predict survival for all individuals of that sex, and predict death if the survival rate 
# for a sex is under 0.5.
# What is the accuracy of this sex-based prediction method on the test set?
linear_fit<- lm(Survived ~ Sex, data = train_set)
titanic_test
predict(linear_fit, test_set) %>% unlist ()
y_hat1<-ifelse(predict(linear_fit, test_set) > 1.50, 0,1) %>% as.factor()

x<-test_set[,1:2] %>% ungroup() %>% group_by(Sex) %>% table() %>% as.data.frame() %>% 
  pivot_wider(.,id_cols = c( "Sex"   ),
              names_from = Survived, 
              values_from = Freq) %>% as.data.frame() 
 colnames(x)[2:3]<-c("died","survived")
  mutate(x,survival_by_class = survived/ ( died + survived ))
 

y_hat1<-ifelse(test_set$Sex == "female",1,0) %>% as.factor()

confusionMatrix(y_hat1,test_set$Survived)







# In the training set, which class(es) (Pclass) were passengers more likely to survive than die? 
# Note that "more likely to survive than die" (probability > 50%) is distinct from "equally likely to survive or die" 
# (probability = 50%).

my_table<-train_set %>% filter(Pclass == "1")%>% summary()
x<-my_table[1:2,1] %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(x)<-c("died","survived")


my_table<-train_set %>% filter(Pclass == "2")%>% summary()
x<-  rbind(x,  my_table[1:2,1] %>% as.data.frame(.)$. )   

  
my_table<-train_set %>% filter(Pclass == "3")%>% summary()
x<-  rbind(x,  my_table[1:2,1] %>% as.data.frame(.)$. ) 
rownames(x)<-c(1:3)

x$Pclass<-c(1:3)

x<-x %>% separate(.,col = died , into = c("junk","died"), sep = ":" ) %>% 
  separate(.,col = survived , into = c("junk","survived"), sep = ":" )
x$junk<- NULL

x$died<-x$died %>% as.numeric()
x$survived<-x$survived %>% as.numeric()

x<-x %>% mutate(.,survival_by_class = survived/ ( died + survived ) )


# Predict survival using passenger class on the test set:
# predict survival if the training set survival rate for a 
# class is over 0.5, otherwise predict death.
# What is the accuracy of this class-based prediction method on the test set?

test_set
x<-merge(test_set, x[,3:4])

 
y_hat2<-ifelse(test_set$Pclass == "1" ,1,  0) %>% as.factor()
confusionMatrix(y_hat2, test_set$Survived)


# Use the training set to group passengers by both sex and passenger 
# class. Which sex and class combinations were more likely to survive than die 
# (i.e. >50% survival)?
# Select ALL that apply.

x<-train_set[,1:3] %>% group_by(Sex, Pclass) %>%  
table() %>% as.data.frame() %>%     
pivot_wider(.,id_cols = c( "Sex"  ,    "Pclass"  ),
            names_from = Survived, 
            values_from = Freq)
colnames(x)[3:4]<-c("died","survived")
x<-x %>% 
mutate(.,survival_by_class = survived/ ( died + survived ) )

x[,c(1,2,5)]


x<-merge(test_set,x[,c(1,2,5)])
 
y_hat3<-ifelse(x$survival_by_class >.5 ,1,0) %>% as.factor()

confusionMatrix(y_hat1,test_set$Survived) %>% F_meas(.,reference = test_set$Survived)
confusionMatrix(y_hat2,test_set$Survived)
confusionMatrix(y_hat3,x$Survived)


F_meas(x$Survived,y_hat3)


#Set the seed to 1. Train a model using Loess with the caret gamLoess method 
# using fare as the only predictor. 
#What is the accuracy on the test set for the Loess model?

library(caret)
set.seed(1)
glm_fit1<- caret::train(Survived ~ Fare,method = "gamLoess", data = train_set)
predict(glm_fit, test_set)   
confusionMatrix(predict(glm_fit, test_set) , test_set$Survived)$overall[1]

 
    set.seed(1)
    glm_fit2<- caret::train(Survived ~ .,method = "glm", data = train_set[,c(1,4)])
    predict(glm_fit2, test_set)  
    confusionMatrix( predict(glm_fit2, test_set), test_set$Survived)$overall[1]


 
       set.seed(1)
       glm_fit3<- caret::train(Survived ~ .,method = "gamLoess",data = train_set[,1:5])
       predict(glm_fit2, test_set)  
       confusionMatrix(predict(glm_fit3, test_set), test_set$Survived)$overall[1]


          set.seed(1)
          glm_fit4<- caret::train(Survived ~ .,method = "glm",data = train_set)
          predict(glm_fit4, test_set)  
          confusionMatrix(predict(glm_fit4, test_set), test_set$Survived)$overall[1]
          
# Set the seed to 6. Train a kNN model on the training set using the caret train function.
# Try tuning with k = seq(3, 51, 2) . 
# What is the optimal value of the number of neighbors k ?
set.seed(6)
knn_fit1<-caret::train(Survived ~., method = "knn",
             data = train_set, 
             tuneGrid = data.frame(k = seq(3, 51, 2)) ) #%>% ggplot(.,highlight = TRUE)


predict(knn_fit1, test_set) %>% 
confusionMatrix(.,test_set$Survived)

      set.seed(8)
      knn_fit2<-caret::train(Survived ~., method = "knn",
                       data = train_set, 
                       tuneGrid = data.frame(k = seq(3, 51, 2)),
                       trControl = trainControl(method = "cv", number = 10, p = .9)) #%>% ggplot(.,highlight = TRUE)


      predict(knn_fit2, test_set) %>% 
      confusionMatrix(.,test_set$Survived)

   
# Set the seed to 10. Use caret to train a decision tree with the rpart method. 
# Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
      set.seed(10)
      tree_fit1<-caret::train(Survived ~., method = "rpart",
                             data = train_set, 
                             tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                             # trControl = trainControl(method = "cv", number = 10, p = .9)
                             ) %>% plot(.)
      
      
      predict(tree_fit1, test_set) %>% 
        confusionMatrix(.,test_set$Survived)
      tree_fit1$finalModel %>% plot()
      tree_fit1$finalModel %>% text(.,cex = .5)
      
      
     x<- train_set[,c(1:3)]   
     x  %>% table()
       
   x$two_sibs<-ifelse(train_set$SibSp == 2 ,"yes","no")
    x$is_seventeen<-ifelse(train_set$Age == 17.00, "yes","no")

    
# Set the seed to 14. Use the caret train() function with the rf 
# method to train a random forest. 
# Test values of mtry = seq(1:7). Set ntree to 100.    
  
  set.seed(14) 
 rf_fit<-train(Survived ~ ., method= "rf",
         data = train_set,
         tuneGrid = data.frame(mtry = seq(1:7) ),ntree =100)
 
 predict(rf_fit,test_set) %>% 
   confusionMatrix(.,test_set$Survived)
      