# Each theory builds on the next
# RECOMMENDATION %>% 
# MATRIX FACTORIZATION %>% 
# SVA %>% 
# PCA %>% 

library(caret)
library(dslabs)
data(movielens)


train_small<-movielens %>% 
  group_by(movieId) %>% 
  filter(n()>=50|movieId == 3252) %>% ungroup() %>% 
  group_by(userId) %>% 
  filter(n()>=50) %>% ungroup()


# Comparing Features
# we make of table with movie ID as the columns
# useID a the rows
# and rating as the values
y<-train_small %>% 
  dplyr::select(userId,movieId,rating) %>% 
  pivot_wider(names_from = "movieId", values_from = "rating") %>% 
  as.matrix()

rownames(y)<-y[,1]
y<-y[,-1]


# based on Movie ID and title
# we see the distinct combinations
# This is a bit easier than seeing the unique values of each
# column. 
movie_titles<-movielens %>% 
  dplyr::select(movieId,title) %>% 
  distinct()


#with is an  evalution of unique movie_titles data frame
# a subset of title that matches the colnames of y are set to
# the colnames of y
# 
colnames(y)<-with(movie_titles,
                  title[match(colnames(y),
                               movieId)])

# this Gives is Residuals  depending on the dimension
# we choose use
y<-sweep(y,1,rowMeans(y,na.rm = TRUE))
y<-sweep(y,2,colMeans(y,na.rm = TRUE))
## if you average these tables together is
# that the linear model residual??????

# it is the Standard error or Deviation for
# multiple dimensional data is characterizes
# by tables of  residual values(Deviation from the mean for that dimension)
# each Dimension is its own table the average of which is deviation
# from the model for all dimensiosn.  
# A Factorization matrix is fold change for this averaged Residual table
# SVD/PCA  is the Algorithm that Characterizes that  matrix
 


#set all NA values to 0
y[is.na(y)]<-0
y<-sweep(y,1,rowMeans(y))
pca<-prcomp(y)

dim(pca$x)
1


# in this object sdev is the s.d of the principal components 
# and also the squre roots of the eigenvalues of the covariance matrix.

rotation is a matrix whose columns contain the eigenvectors, the principal components in the original coordinate system.


#I think this is the SD of all the features
plot(pca$sdev)

var_explained<-cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)


install.packages("ggrepel")
library(ggrepel)

pcs<-data.frame(pca$rotation,name = colnames(y))

pcs %>% ggplot(aes(PC1,PC2)) + geom_point()+
  geom_text_repel(aes(PC1,PC2, label = name),
                  data = filter(pcs,PC1< -.1 | PC1 > .1 | PC2 < .075 | PC2 > .1))
