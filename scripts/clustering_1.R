library(tidyverse)

data <- usvideos %>%
  select(category_id, comment_count, likes, dislikes, views) %>%
  group_by(category_id) %>%
  summarise(average_views = mean(views),
            average_likes = mean(likes),
            average_dislikes = mean(dislikes),
            average_comments = mean(comment_count))
data <- data %>%
  column_to_rownames(var = 'category_id')
df <- data
library(factoextra)
distance <- get_dist(df)
fviz_dist
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df[,(2:4)], centers = 3, nstart = 25)
k2
fviz_cluster(k2, data = df)


df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(data)) %>%
  ggplot(aes(average_views, average_comments, color = factor(cluster), label = state)) +
  geom_text()


###to determine the optimal number of clusters
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



######



df <- scale(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)
k2

k3 <- kmeans(df, centers = 3, nstart = 25)
k3
k4 <- kmeans(df, centers = 4, nstart = 25)
k4
k5 <- kmeans(df, centers = 5, nstart = 25)
k5

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


