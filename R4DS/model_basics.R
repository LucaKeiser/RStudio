
# source: https://r4ds.had.co.nz/model-basics.html


library(tidyverse)
library(modelr)

# NA's are not silently dropped
options(na.action = na.warn)



# A simple model ----------------------------------------------------------

sim1 <- sim1

# sim1-data set
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point()



## random search ----------------------------------------------------------
# run mutiple linear models to get a feeling
models <- tibble(
  a1 = runif(n = 250, min = -20, max = 40),
  a2 = runif(n = 250, min = -5, max = 5)
)


ggplot(sim1, aes(x = x, y = y)) + 
  geom_abline(aes(intercept = a1, slope = a2),
              data = models,
              alpha = 0.25) + 
  geom_point()




# create model-prediction-function (turn model into a function)
model1 <- function(a, data) {
  
  # yhat = b0 + b1 * x
  a[1] + a[2] * data$x
}

# 7 and 1.5 are the parameters of the  1.model!
model1(c(7, 1.5), sim1)




# measure distance between yhat and y with one value
# => “root-mean-squared deviation”
# We compute the difference between actual (y)
# and predicted (yhat), square them, average them, 
# and the take the square root. 

measure_distance <- function(mod, data) {
  
  # difference = y - yhat (= b0 + b1 * x)
  diff <- data$y - model1(mod, data)
  
  # square differecnt, average it and take the square root
  sqrt(mean(diff^2))
}


measure_distance(c(7, 1.5), sim1)



# let's compute the difference for all 250 models above

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

# need a purrr-helper function because 
# our distance function expects the model 
# as a numeric vector of length 2.
models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

View(models)


# plot the 10 best fitting models
ggplot(sim1, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )


# or think of the models as data points (observations)
# again the best 10 are highlighted
ggplot() +
  geom_point(aes(x = a1, y = a2),
             size = 4, 
             color = "red",
             data = filter(models, rank(dist) <= 10)) + 
  geom_point(aes(x = a1, y = a2,
                 color = -dist),
             data = models)



## grid search ------------------------------------------------------------

# instead of fitting models randomly we could do it more 
# systematically

grid <- expand.grid(
  # range is picked from the top 10 models above...
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
  ) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(x = a1, y = a2)) + 
  geom_point(data = filter(grid, rank(dist) <= 10),
             size = 4, 
             color = "red") + 
  geom_point(aes(color = -dist))


ggplot(sim1, aes(x = x, y = y)) + 
  geom_point(size = 2, alpha = 0.5) + 
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

# compare to random search => distances between y and yhat are much smaller!
# You could imagine iteratively making the grid finer and finer until
# you narrowed in on the best model. But there is an even better way...




## Newton-Raphson-search --------------------------------------------------
# numerical minimisation tool

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

# If you have a function that defines the distance between a model and a dataset, 
# an algorithm (optim()) that can minimise that distance by modifying the parameters 
# of the model, you can find the best model. The neat thing about this approach is 
# that it will work for any family of models that you can write an equation for.

ggplot(sim1, aes(x = x, y = y)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = best$par[1], slope = best$par[2])

# looks damn good!



## linear model -----------------------------------------------------------

# y = a_1 + a_2 * x_1 + a_3 * x_2 + ... a_n * x_(n-1)

sim1_mod <- lm(y ~ x, data = sim1)

coef(sim1_mod)

# These are exactly the same values we got with optim()! 
# Behind the scenes lm() doesn’t use optim() but instead 
# takes advantage of the mathematical structure of linear 
# models. Using some connections between geometry, 
# calculus, and linear algebra, lm() actually finds the 
# closest model in a single step, using a sophisticated 
# algorithm. This approach is both faster, and guarantees 
# that there is a global minimum.




## Exercises --------------------------------------------------------------

# 1)
# One downside of the linear model is that it is **sensitive to 
# unusual values because the distance incorporates a squared 
# term**. Fit a linear model to the simulated data below, and 
# visualise the results. Rerun a few times to generate different 
# simulated datasets. What do you notice about the model?


# student t-distribution
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
  )


ggplot(sim1a, aes(x= x, y = y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# compare to normal distribution
sim1b <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 * rnorm(length(x), mean = 0, sd = 1)
  )
          

ggplot(sim1b, aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# the probability to get outliers is higher with the students-t-distribution
# why?
# students-t-distribution has heavier tails than the normal distribution!
# probability for outliers ist higher!

tibble(
  x = seq(-5, 5, length.out = 5000),
  normal = dnorm(x),
  student_t = dt(x, df = 2)
) %>% 
  pivot_longer(cols = c(normal, student_t), 
               names_to = "distribution",
               values_to = "densitiy") %>% 
  ggplot(aes(x = x, y = densitiy, colour = distribution)) +
  geom_line()



# 2)
# One way to make linear models more robust is to use a different distance 
# measure. For example, instead of root-mean-squared distance, you could 
# use mean-absolute distance:

measure_distance_2 <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}


# Use optim() to fit this model to the simulated data above and compare 
# it to the linear model.


# Newton-Raphson search
newton_raphson <- optim(c(0, 0), measure_distance_2, data = sim1a)
newton_raphson$par


ggplot(sim1a, aes(x = x, y = y)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = newton_raphson$par[1], slope = newton_raphson$par[2])


# linear model
linear_model <- lm(y ~ x, data = sim1a)
linear_model$coefficients

ggplot(sim1a, aes(x = x, y = y)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = linear_model$coefficients[1],
              slope = linear_model$coefficients[2])



# compare to measure_distance
newton_raphson_2 <- optim(c(0, 0), measure_distance, data = sim1a)
newton_raphson_2$par

# coefficients the same as the lm-coefficients!






# Visualising models ------------------------------------------------------


## Predictions ------------------------------------------------------------

# predictors tell us the pattern, the model has captured

# take a look at the sim1 data again
View(sim1)


# create a grid
grid <- sim1 %>% 
  data_grid(x)

grid


# add predictions (1 perdiction per x-value)
grid <- grid %>% 
  add_predictions(sim1_mod)

grid


# plot
ggplot() +
  geom_point(data = sim1, aes(x = x, y = y)) +
  geom_line(data = grid, aes(x = x, y = pred),
            color = "red")




## Residuals (flip side of predictors) ------------------------------------

# residuals tell you what the model has missed
# residuals = y (observed values) - yhat (predicted values)

# we need the original data set to calculate the residuals!
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)

sim1


# how spread are the residuals?
ggplot(sim1, aes(x = resid)) +
  geom_freqpoly(binwidth = 0.5)

# mean of the residuals will always be 0
mean(sim1$resid)

ggplot(sim1, aes(x = x, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()


# Putting a reference line at zero for residuals is important because 
# good models (generally) should have residuals centered at zero, with 
# approximately the same variance (or distribution) over the support of x, 
# and no correlation. A zero reference line makes it easier to judge these 
# characteristics visually.




## Exercises  -------------------------------------------------------------

# Instead of using lm() to fit a straight line, you can use loess() to fit 
# a smooth curve. Repeat the process of model fitting, grid generation, 
# predictions, and visualization on sim1 using loess() instead of lm(). 
# How does the result compare to geom_smooth()?


sim1_loess <- loess(y ~ x, data = sim1)
sim1_lm <- lm(y ~ x, data = sim1)


sim1 <- sim1 %>% 
  add_residuals(sim1_lm) %>% 
  add_predictions(sim1_lm) %>% 
  add_residuals(sim1_loess, var = "resid_loess") %>% 
  add_predictions(sim1_loess, var = "pred_loess")


# plot
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = pred_loess), color = "red")

# this is the standard method of geom_smooth...
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(se = FALSE)


# compare the residuals
ggplot(data = sim1) +
  geom_ref_line(h = 0) + 
  # lm
  geom_point(aes(x = x, y = resid), color = "red") +
  # loess (in genereal smaller residuals within (!) the sample)
  geom_point(aes(x = x, y = resid_loess), color = "blue")






# Categorical variables ---------------------------------------------------

# dummy-trap
# You might wonder why R also doesn’t create a sexfemale column. The problem 
# is that would create a column that is perfectly predictable based on the 
# other columns (i.e. sexfemale = 1 - sexmale). Unfortunately the exact details 
# of why this is a problem is beyond the scope of this book, but basically it 
# creates a model family that is too flexible, and will have infinitely many 
# models that are equally close to the data.

# in the background R creates a reference category
model_matrix(sim3, y ~ x1 + x2)


sim2 <- sim2

# plot
ggplot(sim2, aes(x = x , y = y)) +
  geom_point()


# generate model
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)

grid



# Effectively, a model with a categorical x will predict 
# the mean value for each category!
ggplot(data = sim2, aes(x = x, y = y)) +
  geom_point() +
  geom_point(data = grid, aes(x = x, y = pred), 
             color = "red",
             size = 2)





# Interactions (continuous and categorical) -------------------------------

sim3 <- sim3

ggplot(data = sim3, aes(x = x1, y = y)) +
  geom_point(aes(color = x2))


# 2 possible models
mod1 <- lm(y ~ x1 + x2, data = sim3)

# y ~ x1 * x2 is translated to y = a_0 + a_1 * x1 + a_2 * x2 + a_12 * x1 * x2
# Note that whenever you use *, both the interaction AND 
# the individual components are included in the model.
mod2 <- lm(y ~ x1 * x2, data = sim3)




