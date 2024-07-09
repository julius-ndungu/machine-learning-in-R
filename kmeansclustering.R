# importing the data

data <- read.csv("train.csv")
head(data)
#simplify data
data <- data[,2:9]
str(data)
data[data == ""] <- NA
# data manipulation

library(tidyverse)
data <- data %>% 
  filter(complete.cases(.))
# selecting the numeric column
data_numeric <- data %>% select_if(is.numeric)
names(data_numeric)
# selecting the charactercolumns
data_character <- data %>% select_if(is.character)
names(data_character)
head(data_character)
unique(data$Profession)
#transforming character to dummy
library(fastDummies)
data_character <- dummy_cols(data_character,
                             remove_most_frequent_dummy = TRUE)
data_character <- data_character %>% select(-c(1,2,3,4,5))
dataset <- cbind(data_numeric, data_character)
# transformed data

head(dataset)
#scale the data
data_scale <- data.frame(scale(dataset))
head(data_scale)
library(factoextra)
#determining the number of clusters
set.seed(111)
fviz_nbclust(data_scale, kmeans, method = "wss")+
  labs(subtitle = "Elbow Method")

# conducting kmeans clustering

cluster <- kmeans(data_scale, centers = 6)
#  Add cluster assignment to the original dataset
dataset$cluster <- as.factor(cluster$cluster)
head(dataset)

# Summarize the characteristics of each cluster
cluster_summary <- dataset  %>%
  group_by(cluster) %>%
  summarise_all(list(mean = mean, sd = sd))

print(cluster_summary)

## Example two


us <- datasets::USArrests
us_scale <- scale(us)
head(us_scale)
# optimum nuber of clusters

fviz_nbclust(us_scale, kmeans, method = "wss")
u_k <- kmeans(us_scale, centers = 4)
u_k$cluster
us$cluster <- u_k$cluster
## summary of the clusters
us  %>%
  group_by(cluster) %>%
  summarise_all(list(mean = mean, max = max))

## ploting the clusters

fviz_cluster(u_k, us, ellipse.type =  "convex")
