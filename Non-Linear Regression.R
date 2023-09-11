# http://rafalab.dfci.harvard.edu/dsbook/smoothing.html
library(tidyverse)
library(dslabs)
library(gridExtra)
library(stats)

set.seed(1)
#Example of smooth data and with noise added in
n <-100
x<-seq(-pi*4, pi*4, len = n)
tmp<-data.frame(x=x, f=sin(x) + x/8, e = rnorm(n,0,0.5) )
p1<-qplot(x,f,main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp,geom = "line")
p2<-qplot(x,e,main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3<-qplot(x,f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")

gridExtra::grid.arrange(p1,p2,p3)


#bin smoothers
span<-3.5
tmp<-polls_2008 %>% 
  crossing(center = polls_2008$day) %>% 
  mutate(dist = abs(day-center)) %>%
  filter(dist <= span)


tmp %>% filter(center %in%  c(-125,-55)) %>% 
  ggplot(aes(day,margin))+
  geom_point(data = polls_2008,size =3 ,alpha = .5, color = "grey")+
  geom_point(size=2)+ #????????????
  geom_smooth(aes(group = center), method = "lm", formula = y~1,se = FALSE)+
  facet_wrap(~center)




# larger span
span <- 7
# this is similar to lm but uses the ksmooth algorithym to
# transform the data
fit <- with(polls_2008, ksmooth(day,margin,kernel = "normal", bandwidth = span) )

polls_2008 %>% mutate(smooth = fit$y ) %>% 
  ggplot(aes(day,margin))+
  geom_point(size =3, alpha = .5, color = "grey")+
  geom_line(aes(day,smooth),color = "red")



# using a linear method ie LOESS is a better way than
# bin smoothing because a line is drawn instead of taking 
# a point 
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

#when using loess the fitted column is the y_hat
polls_2008 %>%  mutate(smooth = fit$fitted) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

#######################################HOMEWORK######################################

library(tidyverse)
library(purrr)
library(lubridate)
install.packages("pdftools")
library(pdftools)
fn<-system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",package = "dslabs")
df<-map_df(str_split(pdf_text(fn), "\n"),function(s) {
  s<-str_trim(s)
    header_index<-str_which(s, "2015")[1]
    tmp<-str_split(s[header_index],"\\s+", simplify = TRUE)
    month <- tmp[1]
    header<- tmp[-1]
    tail_index <- str_which(s,"Total")
    n <- str_count(s,"\\d+")
    out<- c(1:header_index, which(n==1), which(n>=28),tail_index:length(s))
    s[-out] %>% 
      str_remove_all("[^\\d\\s]") %>% 
      str_trim() %>% 
        str_split_fixed("\\s+",n=6) %>% 
        .[,1:5] %>% 
        as_tibble() %>% 
        setNames(c("day",header)) %>% 
        mutate(month=month, day = as.numeric(day)) %>% 
        gather(year,deaths,-c(day,month)) %>% 
        mutate(deaths = as.numeric(deaths))
}) %>% 
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year,month,day)) %>% 
   dplyr::filter(date <= "2018-05-01")


#one row had an NA always check
dat<-na.omit(df)



#using the date and death variable..... fit a model 
total_days <- diff(range(dat$date))
span <- 60/1216
fit <- loess(deaths ~ as.numeric(dat$date), degree = 1, span = span, data=dat)
       dat<-dat %>%  mutate(smooth = fit$fitted)

       
#using day and death variables..... fit a model
# for the span....note there are three years each having 12 months each having 30 days       
dat$day2<-1:dim(dat)[1]
span2<-60/diff(range(dat$day2))
fit2<-loess(deaths ~ day2, degree = 1, span = span2, data=dat)
  dat<-dat %>%  mutate(smooth2 = fit2$fitted)
 

dat<-ungroup(dat)

# the fitted column from the loess function gives us y_hats to use and plot
ggplot(data = dat,aes(x=date,y=deaths, size = 1, alpha = .5))+
  geom_point()+
  geom_smooth(stat = "smooth", color = "red", size =1)+
  geom_line(aes(date,smooth),color = "blue", size =1)+
  geom_line(aes(date,smooth2),color = "green", size =1)+
  theme_bw()+labs(title = "LOESS fit with ggplot = red, date = blue and day = green ")

dat %>% 
  mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)



#in the 2 and 7 problem we can make a linear model from the training data
library(broom)
mnist_27$train %>% glm(y~x_2,family = "binomial",data = .) %>% tidy()
qplot(x_2,y,data = mnist_27$train)


# we can also use loess to maked a model 
# the smaller the span the more granular we get
# categorical data must be switched to 0 and 1 so we can get
# probablitities
fit2 <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  #lm(y ~ x_1 + x_2, data = .)
loess(formula = y~x_2,degree =1)


fit<-loess(data = mnist_27$train, formula = as.numeric(y) ~ x_2, degree =1, span = .09)



# fit has predicted values in the fitted column but this
# is from the training data set
# lets get the predicted values from the test set
predict(fit2, mnist_27$test[3], type = "response")
df<-data.frame(mnist_27$test,predict(fit2, mnist_27$test[3]))
colnames(df)[4] = "p_hat"
df<-df %>% mutate( y_hat = ifelse(p_hat >= .5 ,7,2))

# since our predicted y_hat is binomial in nature we need to transform
# the the fitted y_hat values into yes or no answer
# to do this they must be turned into probabilities
# and then a cut off can be set.
confusionMatrix(as.factor(df$y),as.factor(df$y_hat))

ggplot(data = mnist_27$test, aes(x=x_2, y = y))+
  geom_point()+
  geom_line(aes(x=df$x_2,y = df$predict.fit2..mnist_27.test.3.. ),  color= "blue")+
  geom_smooth(stat = "smooth", color= "red")+
  theme_bw()+labs(title = "2 vs 7")
  
