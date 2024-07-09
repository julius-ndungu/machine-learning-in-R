getwd()
library(tidyverse)
library(sjPlot)
library(robustbase)
library(olsrr)
#linear regression model
#the relationship between speed and stopping distance
head(cars,20)
?cars
# simple linear regression

mod <- cars %>% 
  lm(dist ~ speed, data = .)
## summary of the model

summary(mod)
## checking outliers

ols_plot_resid_lev(mod)
plot_residuals(mod2)
# robust linear regression(taking into account the outliers)

mod2 <- lmrob(dist ~ speed, data = cars)
# summary table

summary(mod2)

plot_residuals(mod2)
#predicting new speed with 95 confidence

new_speed <- data.frame(speed = c(10,15,20))

predict(mod2, new_speed, interval = "confidence") %>% round(1)

