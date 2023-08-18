# This Script selects which feature in the iris Data set
# is the best predictor for species
# it also look for an optimal cutoff for that predictor
library(dslabs)
library(dplyr)
library(lubridate)
install.packages("plotly")
library(plotly)
data("reported_heights")
 

# On 2016-01-25 at 8:15 AM there was an online survey
#this filter the data and labels who was online and who wasnt
dat<-mutate(reported_heights,date_time = ymd_hms(time_stamp)) %>% 
     filter(date_time >= make_date(2016,01,25) & date_time < make_date (2016,02,1)) %>% 
     mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time),15,30), "inclass" , "online" )) %>% 
     select(sex,type)

y <- factor(dat$sex, c("Female","Male"))
x <- dat$type


# a table replects the numbers of each group
table(y,x)
(26/(26+13))*100

(42/(42+69))*100

# we can also group and then summarize
dat %>%  
group_by(type) %>% 
summarize( prop_female= mean(sex == "Female")) #setting this to boolean allows mean to count the instances


# lets set cut offs for y_hat if "inclass" is true y_hat is female else "online" male
y_hat <- ifelse(dat$type == "inclass", "Female","Male" )
 

# now we compare that rule to the original data
confusionMatrix(factor(y_hat), factor(dat$sex))
confusionMatrix(y,factor(x))


###############################################################################
#in this section we take the iris data set 
# based on the mean of each feature we use that as a cutoff
# to predict species
# a confusion matric less us see which cutoff is best
# then we set a scale and test each incremental increase
# to see which is valueis best within that feature
#########################################################################

#were only interested in non setosa species
data(iris)
iris<-iris[-which(iris$Species == 'setosa'),]
y<-iris$Species

# splitting the data 50/50 first we create a partion that randomly selects rows  and 
# saves the index, its just a key really
set.seed(76)
test_index<-createDataPartition(y, times = 1,p = 0.5, list = FALSE)

x_Train<-iris[test_index,]
x_Test<-iris[-test_index,]

 
# To get averages for all columns acros groups
df<-x_Train %>% as.data.frame() %>% 
    gather(.,key="metric", value = "cm", c(1:4)) %>% 
    group_by(., metric) %>%
    summarise(mean = mean(cm))
  

# now we can generate data for each metric based on its cutoff
# and pipe it into a confusion matrix with the the training set

y_hat_PL<-
  
factor(ifelse(x_Train$Petal.Length >= 4.90, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Train$Species))

factor(ifelse(x_Train$Petal.Width >= 1.66, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Train$Species))

factor(ifelse(x_Train$Sepal.Length >= 6.31, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Train$Species))

factor(ifelse(x_Train$Sepal.Width >= 2.88, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Train$Species))






##############################################################################################################
################################################### PETAL Width ####################################################
# Petal Width was the winner so lets findout where in that range
# of values is most accurate
cutoffs_pw<-seq(min(x_Train$Petal.Width), max(x_Train$Petal.Width), by =.1)


# apply to each element of our cutoff a ifelse function
y_hat_pw<-lapply(cutoffs_pw, function(x){
  y_hat<-ifelse(x_Train$Petal.Width >=(x), "virginica","versicolor")
  y_hat
})



# this is a list of y_hat so convert each one to a factor
# and run it thru a cofusion matrix against the training setg
Accs_pw<-lapply(y_hat_pw, function(x){
  factor(x) %>% 
  confusionMatrix(., factor(x_Train$Species))
  
})



# extract all the Accuracies
# format into a table and graph
Accuracies_pw<-lapply(Accs_pw, function(x){
  #i= 1
   (x)$overall['Accuracy']
  #i=i+1
})


Accuracies_pw<-as.data.frame(Accuracies_pw) %>% 
  gather(.,key="key",value="Accuracy",1:14)

Accuracies_pw$key<-cutoffs
colnames(Accuracies_pw)[1]<-"cutoff"


# accuracy for each cutoff the best is 1.7 for the training data
p<-ggplot(Accuracies_pw, aes(x= cutoff,y = Accuracy))+
  geom_point(aes(size=3, alpha=.5))+
  theme_bw()
ggplotly(p)




# we apply this optimal cutoff to the test data and look at our accuracty
# thru another confusion matrix
factor(ifelse(x_Test$Petal.Width >= 1.7, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Test$Species))









 
##############################################################################################################
###################################################PETAL LENGTH####################################################
# lets try the same for petal length since that was runner up
# in our first confusion matrix


# again we make a list of cut offs
cutoffs_pl<-seq(min(x_Train$Petal.Length), max(x_Train$Petal.Length), by =.1)


# there's more but again we
# apply to each element of our cutoff a ifelse function
y_hat_pl<-lapply(cutoffs_pl, function(x){
  y_hat_pl<-ifelse(x_Train$Petal.Length >=(x), "virginica","versicolor")
  y_hat_pl
})



# this is a list of y_hats so convert each one to a factor
# and run it thru a cofusion matrix against the training set
Accs_pl<-lapply(y_hat_pl, function(x){
  factor(x) %>% 
    confusionMatrix(., factor(x_Train$Species))
  
})



# extract all the Ac curacies
# format into a table and graph
Accuracies_pl<-lapply(Accs_pl, function(x){
  #i= 1
  (x)$overall['Accuracy']
  #i=i+1
})


Accuracies_pl<-as.data.frame(Accuracies_pl) %>% 
  gather(.,key="key",value="Accuracy",1:30)

Accuracies_pl$key<-cutoffs_pl
colnames(Accuracies_pl)[1]<-"cutoff"


# accuracy for each cutoff

p1<-ggplot(Accuracies_pl, aes(x=cutoff,y=Accuracy))+
  geom_point(aes(size=3, alpha=.5))+
  theme_bw()
ggplotly(p1)
#shows 4.8 is best cuttoff






# we apply this optimal cutoff to the test data and look at our accuracy
# thru another confusion matrix
factor(ifelse(x_Test$Petal.Length >= 5.0, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Test$Species))




# we can also use multiple cutoffs from both the best and second best
# features averages
################################################################################################################
factor(ifelse(x_Test$Petal.Width >= 1.7 & x_Test$Petal.Length >= 5.0, "virginica","versicolor")) %>% 
confusionMatrix(.,factor(x_Train$Species))