library(tidyverse)
library(sjPlot)
library(datasets)
library(mlbench)
library(rsample)
library(performance)
library(sjPlot)
data <- read.csv("default.csv")
str(data)
data$Loan.Offered <- factor(data$Loan.Offered)
data$Gender <- factor(data$Gender)
data$Job <- factor(data$Job)
data$Credit.History <- factor(data$Credit.History)
data$Purpose<- factor(data$Purpose)
data <-data %>% select(-Status)
data <- data %>% distinct()
view(data)
#fitting the model
set.seed(123)
s <- initial_split(data, strata = Loan.Offered, prop = 0.90)
tr <- training(s)
te <- testing(s)

model <- glm(Loan.Offered ~.,
             data = tr, family = "binomial")
model

# checking the assumptions
check_model(model)
# summary table
summary(model)
tab_model(model)
# prediction
p <- predict(model, newdata = te, type = "response")
te$prediction <- p
te <- te %>% mutate(prediction =ifelse(p >= 0.05, "1","0"))
# 1st 6 prediction
te %>% select(Loan.Offered,prediction) %>% 
  head()
# confusion matrix
te$prediction <- factor(te$prediction)
t <- table(te$Loan.Offered, te$prediction)
# accuracy
sum(diag(t))/sum(t)

