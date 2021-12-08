
# source: https://r4ds.had.co.nz/model-basics.html


library(tidyverse)
library(modelr)

# NA's are not silently dropped
options(na.action = "na.warn")



# A simple model ----------------------------------------------------------

data(sim1)

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




### Exercises -------------------------------------------------------------

# 1)
# One downside of the linear model is that it is **sensitive** to 
# unusual values because the distance incorporates a squared 
# term. Fit a linear model to the simulated data below, and 
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
  normal = dnorm(x, mean = 0, sd = 1),
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


# Newton-Raphson search with measure_distance_2 (!)
newton_raphson_2 <- optim(c(0, 0), measure_distance_2, data = sim1a)
newton_raphson_2$par


ggplot(sim1a, aes(x = x, y = y)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = newton_raphson_2$par[1], slope = newton_raphson_2$par[2])


# linear model
linear_model <- lm(y ~ x, data = sim1a)
linear_model$coefficients

ggplot(sim1a, aes(x = x, y = y)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = linear_model$coefficients[1],
              slope = linear_model$coefficients[2])



# compare to measure_distance
newton_raphson <- optim(c(0, 0), measure_distance, data = sim1a)
newton_raphson$par

ggplot(sim1a, aes(x = x, y = y)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = newton_raphson$par[1], 
              slope = newton_raphson$par[2])


# compare the regression lines of the 3 models
# coefficients and regression line with measure_distance are the same as the lm-coefficients!
# measure_distance_2 is slightly different => calculated with another formula!





# Visualising models ------------------------------------------------------


## Predictions ------------------------------------------------------------

# predictors show us the pattern, the model has captured!

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


# plot => the predictors reduce the model (linear relationship between the data points)
ggplot() +
  geom_point(data = sim1, aes(x = x, y = y)) +
  geom_line(data = grid, aes(x = x, y = pred),
            color = "red")




## Residuals (flip side of predictors) ------------------------------------

# residuals tell you what the model has missed!
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
  geom_ref_line(h = 0, colour = "red") +
  geom_point()


# Putting a reference line at zero for residuals is important because 
# good models (generally) should have residuals centered at zero, with 
# approximately the same variance (or distribution) over the support of x, 
# and no correlation. A zero reference line makes it easier to judge these 
# characteristics visually.




### Exercises  ------------------------------------------------------------

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


sim1

# plot predictors (loess)
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = pred_loess), color = "red", size = 1)

# NOTE: this is the default method of geom_smooth...
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(se = FALSE)

# plot predictors (lm)
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point() + 
  geom_line(aes(x = x, y = pred), color = "red", size = 1)

# NOTE: this is another method of geom_smooth...
ggplot(data = sim1, aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# compare the residuals (!)
ggplot(data = sim1) +
  geom_ref_line(h = 0, colour = "black") + 
  # lm
  geom_point(aes(x = x, y = resid), color = "red") +
  # loess 
  geom_point(aes(x = x, y = resid_loess), color = "blue")

# loess has in general smaller residuals within (!) the sample...




## Categorical variables --------------------------------------------------


# Generating a function from a formula is straight forward when the predictor is 
# continuous, but things get a bit more complicated when the predictor is **categorical**. 
# Imagine you have a formula like y ~ sex, where sex could either be male or female. 
# It doesn’t make sense to convert that to a formula like y = x_0 + x_1 * sex because 
# sex isn’t a number - you can’t multiply it! Instead what R does is convert it to 
# y = x_0 + x_1 * sex_male where sex_male is one if sex is male and zero otherwise:

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)

df

model_matrix(df, response ~ sex)

# **dummy-trap**
# You might wonder why R also doesn’t create a sexfemale column. The problem 
# is that would create a column that is perfectly predictable based on the 
# other columns (i.e. sexfemale = 1 - sexmale). Unfortunately the exact details 
# of why this is a problem is beyond the scope of this book, but basically it 
# creates a model family that is too flexible, and will have infinitely many 
# models that are equally close to the data.


# plot
data(sim2)
View(sim2)
ggplot(sim2, aes(x = x , y = y)) +
  geom_point()


# generate model
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  # again one perditor per x value...
  add_predictions(mod2)

grid


# Effectively, a model with a categorical X-variable will predict 
# the mean value for each category!
# Why? Because the mean minimises the root-mean-squared distance!
ggplot(data = sim2, aes(x = x, y = y)) +
  geom_point() +
  geom_point(data = grid, aes(x = x, y = pred), 
             color = "red",
             size = 2)


# compare to the regression-output!
mod2
# xa is the reference category
# the coefficients tell you the difference between the mean values!
# for example if you switch form xa (mean = 1.15) to xb (mean = 8.12)
# the coefficient is 8.12 - 1.15 = 6.96
# etc.!



## Interactions (continuous and categorical) ------------------------------

data(sim3)

# what happens if you have a contonuous and a categorical predictors?
# lets take a look! x1 = continuous, x2 = categorical
ggplot(data = sim3, aes(x = x1, y = y)) +
  geom_point(aes(color = x2))


# 2 possible models
# 1. 
mod1 <- lm(y ~ x1 + x2, data = sim3)

# 2.
mod2 <- lm(y ~ x1 * x2, data = sim3)
# y ~ x1 * x2 is translated to y = a_0 + a_1 * x1 + a_2 * x2 + a_12 * x1 * x2
# Note that whenever you use *, both the interaction AND 
# the individual components are included in the model.


# visualise these two models
# create a data-grid
grid <- sim3 %>% 
  # now we need both variables!
  data_grid(x1, x2) %>% 
  # gather the predictions form both models in one step
  gather_predictions(mod1, mod2)


View(grid)


# visualise the results for both models
ggplot(data = sim3, aes(x = x1, y = y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(x = x1, y = pred, color = x2)) +
  facet_wrap(~model) +
  labs(title = "Model Comparison (continuous and categorical)",
       subtitle = "mod1 = no interactions | mod2 = interactions")

# Note that the model that uses + (no interaction) has the same slope for each 
# line, but different intercepts. The model that uses * (interaction) has a 
# different slope AND intercept for each line.


# which model perfoms better?
# take a look at the residuals!

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

View(sim3)

sim3 %>% 
  ggplot(aes(x = x1, y = resid, color = x2)) +
  geom_point() + 
  # facet grid (model AND x2)
  facet_grid(~ model ~ x2)

# mod1: patterns are clearly visible (!)
# => mod1 has missed these patterns!

# mod2: no patterns visible
# => mod2 has captured these patterns! 



## Interactions (two continuous) ------------------------------------------

data (sim4)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
# y ~ x1 * x2 is translated to y = a_0 + a_1 * x1 + a_2 * x2 + a_12 * x1 * x2

grid <- sim4 %>% 
  data_grid(
    # not every single value of x is needed here...
    # take a look at the seq_range()-function!
    # => pretty = TRUE, trim, expand...
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>% 
  gather_predictions(mod1, mod2)


grid


# visualise model (first as a 3d surface from the top)

ggplot(data = grid, aes(x = x1, y = x2, fill = pred)) +
  geom_tile() + 
  facet_wrap(~ model) +
  scale_fill_viridis_b() +
  labs(title = "Model Comparison (continuous and continuous)",
       subtitle = "mod1 = no interactions | mod2 = interactions")

# the models do not look that different...



# visualise model (multiple slices from either side)

p1 <- ggplot(data = grid, aes(x = x1, y = pred, color = x2, group = x2)) + 
  geom_line() + 
  facet_wrap(~ model) +
  labs(title = "Model Comparison (continuous and continuous)",
       subtitle = "mod1 = no interactions | mod2 = interactions")


p2 <- ggplot(data = grid, aes(x = x2, y = pred, color = x1, group = x1)) +
  geom_line() + 
  facet_wrap(~ model) +
  labs(title = "Model Comparison (continuous and continuous)",
       subtitle = "mod1 = no interactions | mod2 = interactions")


library(patchwork)
p1 + p2

# An interaction says that there’s not a fixed offset: 
# => you need to consider both (!) values of x1 and x2 simultaneously in order to predict y.
# Interaction: x1 and x2 are connected (if you change one of them, you change the other as well)
# => different slopes AND intercepts!


# You can see that even with just two continuous variables, coming up with good visualisations 
# are hard. But that’s reasonable: you shouldn’t expect it will be easy to understand how 
# three or more variables simultaneously interact!




## Transformations --------------------------------------------------------

# Transformations are useful because you can use them to approximate non-linear 
# functions. If you’ve taken a calculus class, you may have heard of Taylor’s 
# theorem which says you can approximate any smooth function with an infinite 
# sum of polynomials. That means you can use a polynomial function to get 
# arbitrarily close to a smooth function by fitting an equation like 
# y = a_1 + a_2 * x + a_3 * x^2 + a_4 * x ^ 3. 
# Typing that sequence by hand is tedious, so R provides a helper function: poly():

# NOTE: you can always take a look, what R is doing in the "background"

df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)

model_matrix(df, y ~ poly(x, 2))

# Outside the range of the data, polynomials rapidly shoot off to + or - inf
# => be careful with poly()

# better use splines!

library(splines)

model_matrix(df, y ~ ns(x, 2))


# What does it look like when we try to approximate a non-linear (!) model?

# generate the data first!
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  
  # rnorm adds some random noise to the data
  y = 4 * sin(x) + rnorm(n = length(x))
)


# No noise
sim5_NoNoise <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x)
  )



# create models (5 each) & grids
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

mod1_NoNoise <- lm(y ~ ns(x, 1), data = sim5_NoNoise)
mod2_NoNoise <- lm(y ~ ns(x, 2), data = sim5_NoNoise)
mod3_NoNoise <- lm(y ~ ns(x, 3), data = sim5_NoNoise)
mod4_NoNoise <- lm(y ~ ns(x, 4), data = sim5_NoNoise)
mod5_NoNoise <- lm(y ~ ns(x, 5), data = sim5_NoNoise)

# grids
grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5)


grid_NoNoise <- sim5_NoNoise %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1_NoNoise, mod2_NoNoise, mod3_NoNoise, mod4_NoNoise, mod5_NoNoise)



# plot
ggplot(data = sim5, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = grid, aes(x = x, y = pred), color = "red") + 
  facet_wrap(~ model)


ggplot(data = sim5_NoNoise, aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(data = grid_NoNoise, aes(x = x, y = pred), color = "red") +
  facet_wrap(~ model)


# Note the difference between the two data sets!

# Notice that the extrapolation outside the range of the data is clearly bad. 
# This is the downside to approximating a function with a polynomial. But this 
# is a very real problem with every model: the model can never tell you if 
# the behaviour is true when you start extrapolating outside the range of 
# the data that you have seen. 



### Exercises -------------------------------------------------------------

# 1)
# What happens if you repeat the analysis of sim2 using a model without an 
# intercept. What happens to the model equation? What happens to the predictions?

data(sim2)

mod_1 <- lm(y ~ x, data = sim2)
# remove intercept
mod_2 <- lm(y ~ x - 1, data = sim2)

# create a grid
grid <- sim2 %>% 
  data_grid(x) %>% 
  spread_predictions(mod_1, mod_2)

grid
# the predictions are the same!


# coefficients are different!
summary(mod_1)
# with the intercept => xa as reference category
summary(mod_2)
# without the intercept => no reference category



# 2)
# Use model_matrix() to explore the equations generated for the models 
# I fit to sim3 and sim4. Why is * a good shorthand for interaction?

model_matrix(y ~ x1 * x2, data = sim3)
model_matrix(y ~ x1 * x2, data = sim4)


# The asterisk * is good shorthand for an interaction since an interaction 
# between x1 and x2 includes terms for x1, x2, and the product of x1 and x2.



# 3)
# Using the basic principles, convert the formulas in the following two models 
# into functions. (Hint: start by converting the categorical variable 
# into 0-1 variables.)


### mod1
mod1 <- lm(y ~ x1 + x2, data = sim3)

# create function (special case of model_matrix()) => hard coded!
# creating dummy-variables!
model_matrix_mod1 <- function(.data) {
  mutate(.data,
         # using the fact that locigals can be
         # represented with 0s and 1s
         x2b = as.numeric(x2 == "b"),
         x2c = as.numeric(x2 == "c"),
         x2d = as.numeric(x2 == "d"),
         `(Intercept)` = 1) %>% 
    select(`(Intercept)`, x1, x2b, x2c, x2d)
}

model_matrix_mod1(sim3)



# create funciton (special case of model_matrix()) => not hard coded!
model_matrix_mod1b <- function(.data){
  
  # crab the levels of the categorial variable
  lvls <- levels(.data$x2)
  
  # drop the first level (reference categoriy)
  lvls <- lvls[2:length(lvls)]
  
  # create indicator variable for each level of x2
  for (lvl in lvls) {
    
    # new column name x2 + level name
    varname <- str_c("x2", lvl)
    
    # add indicator variable for lvl
    .data[[varname]] <- as.numeric(.data$x2 == lvl)
  }
  
  # generate list of variables to keep
  x2_variables <- str_c("x2", lvls)
  
  # add an intercept
  .data[["(Intercept)"]] <- 1
  
  # keep x1 and x2 indicator variables
  select(.data, `(Intercept)`, x1, all_of(x2_variables))
}

model_matrix_mod1b(sim3)



### mod2
mod2 <- lm(y ~ x1 * x2, data = sim3)

# create function (special case of model_matrix()) => hard coded!
model_matrix_mod2 <- function(.data) {
  mutate(.data,
         `(Intercept)` = 1,
         x2b = as.numeric(x2 == "b"),
         x2c = as.numeric(x2 == "c"),
         x2d = as.numeric(x2 == "d"),
         `x1:x2b` = x1 * x2b,
         `x1:x2c` = x1 * x2c,
         `x1:x2d` = x1 * x2d) %>% 
    select(`(Intercept)`, x1, x2b, x2c, x2d, `x1:x2b`, `x1:x2c`, `x1:x2d`)
}

model_matrix_mod2(sim3)



# create function (special case of model_matrix()) => not hard coded!
# use model_matrix_mod1b()
model_matrix_mod2b <- function(.data){
  
  # get dataset with x1 and x2 indicator variables
  out <- model_matrix_mod1b(.data)
  
  # get names of the x2 indicator columns
  # ^ to match the start of the string.
  x2cols <- str_subset(colnames(out), "^x2")
  
  # create interactions between x1 and the x2 indicator columns
  for (varname in x2cols) {
    
    # name of the interaction variable
    newvar <- str_c("x1:", varname)
    out[[newvar]] <- out$x1 * out[[varname]]
  }
  
  # print 
  out
  
}

model_matrix_mod2b(sim3)



# 4)
# For sim4, which of mod1 and mod2 is better? I think mod2 does a slightly 
# better job at removing patterns, but it’s pretty subtle. Can you come 
# up with a plot to support my claim?

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

# take a look at the residuals
# gahter them first
sim4_mods <- sim4 %>% 
  gather_residuals(mod1, mod2)

sim4_mods


# plot => frequency plot!
ggplot(data = sim4_mods, aes(x = resid, color = model)) +
  geom_freqpoly(bins = 15) +
  geom_rug()

# absolute values
ggplot(data = sim4_mods, aes(x = abs(resid), color = model)) +
  geom_freqpoly(bins = 15) + 
  geom_rug()

# not much difference between the residuals visible...
#  However, mod2 appears to have fewer residuals in the tails 
# of the distribution between 2.5 and 5 (although the most extreme 
# residuals are from mod2.
# => not really clear


# calculate the standard deviations
sim4_mods %>% 
  group_by(model) %>% 
  summarise(sd = sd(resid))

# mod2 has a slightly smaller sd => performs better




## Missing values --------------------------------------------------------

# Missing values obviously can not convey any information about the relationship 
# between the variables, so modelling functions will drop any rows that contain 
# missing values. R’s default behaviour is to silently drop them, but 
# options(na.action = na.warn) (run in the prerequisites), makes sure you get a warning.

# check out exactely how many observations were used
nobs(mod1)
nobs(mod2)



# This chapter has focussed exclusively on the class of linear models, which 
# assume a relationship of the form y = a_1 * x1 + a_2 * x2 + ... + a_n * xn. 
# Linear models additionally assume that the residuals have a normal distribution - 
# which we haven’t talked about - and that the response is continuous. 


# for further information: 
# => https://r4ds.had.co.nz/model-basics.html#other-model-families
