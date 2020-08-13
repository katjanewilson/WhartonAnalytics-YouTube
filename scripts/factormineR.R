
#####Question1: Unsupervised Learning with the top 3000
rm(list = ls())
library(tidyverse)

usvideos <- read.csv("data/UsVideos.csv")

##Visualize the trends for each video

df2 <- usvideos %>%
  select(video_id, views, likes, dislikes, comment_count) %>%
  group_by(video_id) %>%
  summarise(average_views = mean(views),
            average_likes = mean(likes),
            average_comments = mean(comment_count),
            average_dislikes = mean(dislikes))

df2_top <- df2 %>%
  arrange(desc(average_views))
df1 <- df2_top %>%
  remove_rownames() %>%
  column_to_rownames(var = 'video_id')

#select only the top 3,000
df1 <- df1[1:3000,]

library(mclust)
clPairs(df1)

##the pairs show that there is a strange trend with dislike, so perhaps we want to focus on that


#k means way
set.seed(700)
k <- kmeans(df1,centers = 3)
k$centers
table(k$cluster)

##code for the number of clusters that is appropriate
mydata <- df1
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Average total within sum of squares", main = "Total Within SS by Various K")

d<-mydata
library(mclust)
BIC = mclustBIC(d)
plot(BIC)
mod1 = Mclust(d, x=BIC)
plot(mod1, what = "classification")
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.


###Question 2

#split it up into training and testing and do logistic regression on it

#do a logistic regression where the predictor is whether or 
#not it recieves more than 600 million views
#do training and testing on this data set

#create a new column for a binary response, if 1 if over 600 million views
#0 if under 600 million views

views <- usvideos %>%
  arrange(desc(views)) %>%
  group_by(video_id) %>%
  summarise(totalviews = sum(as.numeric(views))) %>%
  arrange(desc(totalviews)) %>%
  mutate(mega = ifelse(totalviews > 10000000, "1", "0"))

table(views$mega)

#merge it back in with the other set

mergedset <- merge(views, usvideos, by = "video_id") 

mergedset$mega <- as.numeric(mergedset$mega)

model1 <- glm(mega ~ views + likes + dislikes + comment_count, data = mergedset)
summary(model1) #dislikes are not predictive of it

plot(model1, residuals = TRUE)
[
##can also add smoothing in here

###then do this again with the training and testing data to see how it runs



###Question 3

#finally, use data story-telling to come up with a product intuitition
#use sentiment analysis here to show what it is 
]