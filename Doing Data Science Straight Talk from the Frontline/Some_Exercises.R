# Cathy O'Neil and Rachel Schutt (2013): Doing Data Science: Straight Talk from the Frontline

# page 70f.


# set seed
set.seed(02102021)

# Simulating fake data
# all ture_-values are **not** observable in reality! 
true_error <- rnorm(n = 2000, mean = 0, sd = 2)
true_beta_0 <- 1.1
true_beat_1 <- -8.2
true_beta_2 <- 10

x_1 <- rnorm(n = 2000, mean = 5, sd = 7)
x_2 <- rgamma(n = 2000, shape = 5, rate = 2)
y <- true_beta_0 + true_beat_1 * x_1 + true_beta_2 * x_2 + true_error


# take a look
hist(y, col = "grey")
hist(x_1, col = "grey")
hist(x_2, col = "grey")

# creating data set
library(tidyverse)

df <- tibble(
  y = y,
  x_1 = x_1,
  x_2 = x_2
)



# Exercise 1 --------------------------------------------------------------

model1 <- summary(lm(y ~ x_1, data = df))
model1

# plot
plot(df$x_1, df$y, pch = 20, col = "red")
abline(model1$coefficients[1], model1$coefficients[2])




# Exercise 2 --------------------------------------------------------------

model2 <- summary(lm(y ~ x_1, data = df))
model2

model3 <- summary(lm(y ~ x_2, data = df))
model3


model4 <- summary(lm(y ~ x_1 + x_2, data = df))
model4


### cross validation
# create training and testing data set with tidymodels

library(tidymodels)

spl <- initial_split(df, prop = 0.8)
train <- training(spl)
test <- testing(spl)


# calculate the mean squared error and compare!
model5 <- summary(lm(y ~ x_1 + x_2, data = train))
mean(model5$residuals^2)


model6 <- summary(lm(y ~ x_1 + x_2, data = test))
mean(model6$residuals^2)



