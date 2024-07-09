poison <- read.csv("poison.csv", header = TRUE)
poison$Education_level <- factor(poison$Education_level, levels = c(0,1,2,3),
                                 labels = c("no education","primary","secondary","higher"))

head(poison)
str(poison)
library(sjPlot)
library(tidyverse)
# EDA
poison %>% 
  plot_frq(Education_level)

view_df(poison)
tail(poison)
#fitting poisson regression

po <- glm(CEB ~ ., data = poison, family = "poisson")
po$family
##summary table

tab_model(po)
# building new dataset

new <- data.frame(Education_level = c("no education", "no education","primary","no education","no education","primary"), birth_age = c(15,15,20,21,21,24))
new$Education_level <- factor(new$Education_level)
new$birth_age <- as.integer(new$birth_age)
new
##predicting new dataset

predict(po, new, type = "response",)
