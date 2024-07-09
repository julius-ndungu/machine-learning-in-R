library(tidyverse)
library(caret)
library(rsample)
library(janitor)
library(randomForest)
library(lime)
#importing the data
loan <- read.csv("Loan_Data.csv")
head(loan)
loan <-loan %>% tibble()
# Replace empty strings with NA in specific columns
loan <- loan %>%
  mutate(
    Gender = na_if(Gender, ""),
    Married = na_if(Married, ""),
    Dependents = na_if(Dependents, ""),
    Education = na_if(Education, ""),
    Self_Employed = na_if(Self_Employed, ""),
    Loan_ID = na_if(Loan_ID, ""),
    Property_Area = na_if(Property_Area, ""),
    Loan_Status = na_if(Loan_Status, "")
  )
## Removing the incomplete cases

loan <- loan %>% filter(complete.cases(.))
## changinf data structures

loan <- loan %>% mutate_at(vars(Gender,Married,Dependents,Education,Self_Employed,
                                Loan_Amount_Term,Credit_History,Property_Area,
                                Loan_Status),factor)
loan <- loan %>% select(-1)
# data dimension
dim(loan)
#EDA
##Gender Distribution

theme_set(theme_test())
loan %>% group_by(Gender) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(Gender,count, fill = Gender))+geom_col()+theme(
    legend.position = "none"
  )+labs(
    y = "Count"
  )+geom_label(aes(label = count))+
  labs(title = "Gender Distribution in Loan Applications")
#other visualization

  
loan %>% 
  ggplot(aes(Married))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Dependents))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Education))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Self_Employed))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Loan_Amount_Term))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Credit_History))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Property_Area))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(Loan_Status))+geom_bar(fill = "blue")
loan %>% 
  ggplot(aes(LoanAmount))+geom_histogram(bins = 10)
summary(loan)

## balancing the data

set.seed(123)
loan <- upSample(loan, loan$Loan_Status)
loan <- loan %>% select(-Class)
split_loan <- initial_split(loan, prop = 0.8, strata = Loan_Status)
loan_train <- training(split_loan)
loan_test <- testing(split_loan)
tabyl(loan_test$Loan_Status)
tabyl(loan_train$Loan_Status)
## random forest model

set.seed(123)

cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)
set.seed(123) 

forest <- train(Loan_Status ~., data = loan_train,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)
forest
plot(forest)
plot(varImp(forest))
#prediction
p  <- predict(forest, newdata = loan_test)
confusionMatrix(p, reference = loan_test$Loan_Status, positive = "Y")
## Explaining the first 3 prediction

explainer <- lime(loan_test[1:3,], forest, n_bins = 4, use_density = T,
                  bin_continuous = T)
explanation <- explain( x = loan_test[1:3,], 
                        explainer = explainer, n_labels = 1,
                        n_features = 5)
plot_features(explanation)











