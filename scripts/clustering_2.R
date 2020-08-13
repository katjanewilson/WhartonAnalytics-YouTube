rm(list = ls())
library(tidyverse)

usvideos <- read.csv("data/UsVideos.csv")
summary(usvideos)
head(usvideos)

#there are 6,282 unique videos
df2 <- usvideos %>%
  select(category_id, views, likes, dislikes, comment_count, category_id, video_id) %>%
  group_by(video_id, category_id) %>%
  summarise(average_views = mean(views),
            average_likes = mean(likes),
            average_comments = mean(comment_count),
            average_dislikes = mean(dislikes))
df2 <- distinct(df2, video_id, .keep_all = TRUE)

#sort by numbers of views
df2 <- df2 %>%
  arrange(desc(average_views))

mean(df2$average_views)
#three splits of the data

df2_1 <- df2[2000:4000,] %>%
  arrange(desc(average_comments))

df2_1 <- df2_1[500:1500,]
# df2_d <- df2[2001:4000,]
# df2_3 <- df2[4001:6281,]

df <- df2_1

df <- df %>%
  column_to_rownames(var = 'video_id')

rescale_df <- df %>%
  mutate(comment_scal = scale(average_comments),
         dislike_scal = scale(average_dislikes),
         likes_scal = scale(average_likes),
         views_scal = scale(average_views)) %>%
  select(-c(average_comments, average_dislikes, average_likes, average_views, category_id))


pc_cluster <-kmeans(rescale_df, 6)
pc_cluster
pc_cluster$centers

pc_cluster_2 <-kmeans(rescale_df, 3)

pc_cluster_2$size

center <-pc_cluster_2$centers
center

fviz_cluster(pc_cluster_2, data = rescale_df)

pc_cluster_2

######now that we've clustered the scaled videos

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(rescale_df[c(1,2)], col=pc_cluster$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(rescale_df[c(3,4)], col=pc_cluster$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)



