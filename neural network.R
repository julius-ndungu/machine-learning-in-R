library(nnet)
library(neuralnet)
library(rsample)
iris
set.seed(123)
s <- initial_split(iris, prop = 0.8, strata = Species )
train <- training(s)
test <- testing(s)
# scaling the data
n <- scale(train[,1:4])
f <- class.ind(train[,5])
head(f)  
n_data <- cbind(n,f)
#modeling
net <- neuralnet(setosa+ versicolor+ virginica ~Sepal.Length +Sepal.Width+ Petal.Length +Petal.Width,
                 data = n_data,
                 threshold = 0.01,
                 hidden = c(4,2),
                 stepmax = 5000,
                lifesign = "full",
                lifesign.step = 100,
                linear.output = FALSE,
                err.fct = "sse")
plot(net)
#predicting
pred <- predict(net, scale(test[,1:4]))
head(pred)
head(test)
colnames(pred)<- c("setosa","versicolor","virginica")
pred2 <- apply(pred,1, which.is.max)
head(pred2)
predicted <- data.frame(Species = pred2)
library(dplyr)
predicted <- predicted %>% 
  mutate(Species = ifelse(Species == 1, "setosa",
                          ifelse(Species == 2, "versicolor", "virginica")))
head(predicted)
table(test$Species, predicted$Species)
29/30
