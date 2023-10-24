library(caret)
set.seed(755)


#take 20 percent of the data and sample it
test_index<-createDataPartition(y=movielens$rating,times=,
                                p=.2,list = FALSE)

# subset that isn;t those 20 slots is training
train_set<-movielens[-test_index,]
# subst that is those 20 slots is test data
test_set<-movielens[test_index,]


# to make sure that there are no movie user combinations in the train set
# that don't also occur in the test set we use semijoin
# otherwise the test_set will have combinations our model will have never seen

test_set %>% semi_join(train_set, by="movieId") %>% 
  semi_join(train_set, by = "userId")

 
dim(train_set)

# Damn thats alot of users...
unique(train_set$userId)
unique(train_set$rating)
unique(train_set$title)
# with raitings from 0 to 5
unique(train_set$genres)

#



# BAsically 700 users are giving ratings on 8000 titles across 900 generes
# of course users are voting differently but lets assume they can only


# if all users give the same rating for all movies 
# then the average of that rating (u) is in essence the least squares estimate  

#              u - Average rating per movie(Bi)
      # if all users give the same rating for PER movie
      # then we would have SEVERAL averages for each movie 
      # deviation from the mean is one kinds of error

#                     u - Average rating per movie per USER Differences(Bu)
              # if each user gives and individual rating per movie
              # now we have and average of each movie for each user
              # deviation from the mean is another kind of error


#                         e is the independent error from a normal distribution
#                          in this case its an average of 2 dimensions                              

# adding these errors up we get a loss function
# Y = u + Bi+ Bu + Eui 


# For the RMSE function the average gives us the
# smallest average error
RMSE<-function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



mean(test_set$rating) %>% 
RMSE(test_set$rating,.)
 



#                               STANDARD ERRORS
# for all movies the mean of the (true rating - the Average rating)
mu<-train_set %>% summarize(mu= mean(rating- mean(train_set$rating)))

#                                  Bi
#grouped by each movie the mean of the (true rating - the Average rating)
Bi<-train_set %>% group_by(movieId) %>% 
  summarize(Bi= mean(rating- mean(train_set$rating))) %>% #movie error
  as.data.frame()
ggplot(Bi, aes(x=Bi))+
stat_bin(bins = 10)

#                                  Bu
# can we group by user and have errors as well?
#grouped by each User the mean of the (true rating - the Average rating)
Bu<-train_set %>% group_by(userId) %>% 
  summarize(Bu= mean(rating- mean(train_set$rating))) %>% #user error
  as.data.frame()
ggplot(Bu, aes(x=Bu))+
  stat_bin(bins = 30)   
   



# the Average rating plus the errors associated with each movie, each user
# and each movie and user together
# should get us closer than just the average rating
# Let’s see how much our prediction improves in terms of RMSE  
fit_movies <- data.frame(movieId = as.integer(test_set$movieId),rating = test_set$rating, 
                          u = mean(test_set$rating) ) %>% 
               merge(.,Bi) %>% merge(.,Bu) %>% 
               mutate(pred1 = u + Bi, pred1 = u + Bu , pred3 = u + Bi + Bu)

RMSE(fit_movies$pred1, fit_movies$u)



# terms can be added to linear models also................
# adding a feature to our model for a linear relationship
# theres several userID's but it treating it as a single average
fit<-lm(rating ~ (userId), data = movielens[1:100,])

#### a Linear model for each userID (2 dimensions)
fit<-lm(rating ~ as.factor(userId), data = movielens[1:100,])

#### a Linear model for each userID ~700 (2 dimensions)
fit<-lm(rating ~ as.factor(userId), data = movielens)


#### a Linear model for each userID ~700 and MovieID ~8000  (3 dimensions)
fit<-lm(rating ~ as.factor(userId) + as.factor(movieId), data = movielens)




#### Q1
# Compute the number of ratings for each movie and then plot it 
# against the year the movie came out using a boxplot for each year. 
# Use the square root transformation on the y-axis (number of ratings) 
# when creating your plot.

# What year has the highest median number of ratings?

movielens %>% group_by(movieId,year)%>%summary(rating_no = count(rating))
movielens %>% group_by(movieId,year)%>%summarise(rating_no = count(rating), median_rating = median(rating)) %>% summary()

# loads the data without assigning a variable

data("movielens")
x<-movielens %>%  
  group_by(movieId,year )%>%  
  summarise(.,rating_no =  count(rating),median_rating = median(rating)) %>% as.data.frame()
p<- x %>%  ggplot(.,aes(x=year,y=median_rating,color = year))+
  geom_col() 
  ggplotly(p)  
  
  class(movielens$title)
  
  
 movielens%>%filter(., grepl("Shawshank", title, ignore.case = TRUE)) 
  
  
x<-movielens %>%  
  group_by(movieId)%>% 
  summarise(.,rating_no =  count(rating),mean_rating = mean(rating))%>%
   as.data.frame()  %>% 
  merge(.,movielens[,c(1:3)]) %>% 
  group_by(title) %>%filter(., grepl("Shawshank", title, ignore.case = TRUE)) %>% 
  as.data.frame()
p<- x %>%  
  ggplot(.,aes(x=year,y=median_rating ,color = year))+
  geom_bar(stat = "identity")   
  
  ggplotly(p)
  
  
  # average rating per year
  # official answer calculates the average over all years from 1993 till 2018. 
  # Both seem to me as proper answers and depend on how one understands the question.
#  gump was only rated in 1994  
  
#  Suppose years is the number of years the movie has been out 
#  (from release up until 2018) and n is the number of ratings the movie has. 
#  The average number of ratings per year is then n/years.
  
 
# n is the number of ratings the movie has. The average number of ratings per year is then n/years.
# Suppose years is the number of years the movie has been out (from release up until 2018) and 
# Note that the year variable is in the dataset is the year that the movie was released.
 
 x<- movielens %>% 
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2018 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    top_n(25, rate) %>%
    arrange(desc(rate)) %>% as.data.frame()
 
 movielens %>%   filter(year >= 1993)%>% 
   group_by(.,year,title) %>% summarise(., mean_rating= mean(rating)) %>% 
   merge(.,x) %>% ggplot(.,aes(x=mean_rating,y=rate))+
   geom_point()
    
    #filter(movielens , grepl("Forrest Gump", title, ignore.case = TRUE)) %>% 
   
 
 library(lubridate)
 movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens$date 

x<-movielens %>% 
  mutate(secs_per_week = 24*60*60*7, weeks=timestamp/(24*60*60*7)%>% round())  %>% 
  group_by(.,weeks) %>% 
  summarise(., avg_rating_week = mean(rating)) %>% as.data.frame
x$weeks<-  round(as.numeric(x$weeks), 0)
 
  ggplot(x,aes(x=weeks,y=avg_rating_week))+
  geom_bar(stat="identity")

x<- movielens %>% group_by(., genres) %>% 
 summarise(., rating_count = length(rating)) %>% 
  filter(., rating_count>1000) %>% 
  merge(.,movielens) %>% 
  group_by(.,genres) %>% 
  mutate(.,mean_rating = mean(rating))

ggplot(x,aes(x=genres,y=mean_rating))+
  geom_bar(stat="identity")

str(x)
x<-movielens %>% filter(., genres == "Action")
x$rating %>% n()

filter(. , grepl("Action|Adventure|Animation", genres,fixed = TRUE)) 

  
  
  x %>% group_by(.,weeks %>% as.character()) %>% summarise(., mean_rating = mean(x$avg_rating_week) %>% as.numeric)
