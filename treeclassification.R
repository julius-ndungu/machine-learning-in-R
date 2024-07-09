library(ISLR)
data(package="ISLR")
car<-Carseats#data set from the library
high<-ifelse(car$Sales<=8,"no","yes")#this will help to create another column
View(high)
high
cars<-data.frame(car,high)#this will form the data frame with additional column high. you can also use mutate functions
View(cars)
str(cars)
as.factor(cars$high)->cars$high#classifying the high from characters to factors
str(cars)
library(tree)# help to build the model
cars$Sales=NULL# help in removing sales column from my data
head(cars)
tree.cars<-tree(cars$high~.,data=cars)# tree classification model
summary(tree.cars)
plot(tree.cars)# plotting the model
text(tree.cars,pretty = 0)# labliling my diagram
#need more on explanation
#key point, we need to divide our data into two
# training and test data
library(caTools)
?sample.split()
sample.split(cars$high,SplitRatio = 0.65)->split_tag;split_tag
#assigns true and false randomly  65% will be true
subset(cars,split_tag==TRUE)->train
subset(cars,split_tag==FALSE)->test
tree(high~.,data = train)->model1
plot(model1)
text(model1,pretty = 2)
predict(model1,test,type = "class")->tree_predict
tree_predict#predict if yes or no
#test for accuracy
table(test$high,tree_predict)
#calculating accuracy
(55+74)/(74+9+2+55)

#other examples


library(rpart)
# data to be sed
Default
str(Default)
summary(Default)
# building a tree classification model
library(rsample)
set.seed(123)
sp <- initial_split(Default, prop = 0.80, strata = default)
t <- training(sp) 
test <- testing(sp)
# tree classification model
library(caret)
#tree1
set.seed(123)
t_c <- rpart(default ~., data = t,control = rpart.control(cp = 0.05))
#prediction
pre <- predict(t_c, test, type = "class")
## accuracy

confusionMatrix(pre,test$default, positive = "No")


# tree2
set.seed(123)
tree2 <- tree(default ~., data = t)
#prediction
pre2 <- predict(tree2, test, type = "class")
## accuracy

confusionMatrix(pre2,test$default, positive = "No")

## tree3
library(party)

set.seed(123)
tree3 <- ctree(default ~., data = t)
#prediction
pre3 <- predict(tree3, test, type = "response")
## accuracy

confusionMatrix(pre2,test$default, positive = "No")

