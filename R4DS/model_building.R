
############################## MODEL BUILDING ################################

# source: https://r4ds.had.co.nz/model-building.html

# We will take advantage of the fact that you can think about a model 
# partitioning your data into pattern and residuals. We’ll find patterns 
# with visualisation, then make them concrete and precise with a model. 
# We’ll then repeat the process, but replace the old response variable 
# with the residuals from the model. The goal is to transition from 
# implicit knowledge in the data and your head to explicit knowledge 
# in a quantitative model. This makes it easier to apply to new domains, 
# and easier for others to use.


# load packages -----------------------------------------------------------

library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)


### Why are low quality diamonds more expensive?

diamonds %>% 
  ggplot(aes(cut, price)) +
  geom_boxplot()


diamonds %>% 
  ggplot(aes(color, price)) +
  geom_boxplot()


diamonds %>% 
  ggplot(aes(clarity, price)) +
  geom_boxplot()


# it looks like we miss something here
# there has to be a confounding variable!
# the weight (carat) of the diamond!
# The weight of the diamond is the single most important factor for determining 
# the price of the diamond, and lower quality diamonds tend to be larger.

diamonds %>% 
  ggplot(aes(carat, price)) + 
  geom_hex(bins = 50)



# modeling ----------------------------------------------------------------

## A first model ----------------------------------------------------------

# we can see which attributes of the diamond affect the relative price 
# by fitting a model to separate out (!) the effect of carat. 

# prepare the data first!

diamonds %>% 
  ggplot(aes(carat)) +
  geom_histogram(bins = 10)

diamonds2 <- diamonds %>% 
  # take care of outliers
  filter(carat <= 2.5) %>% 
  # transform the variables 
  # => makes the relationship linear
  # => and the distribution symmetrical
  mutate(price_log = log2(price),
         carat_log = log2(carat))


# with these adjustments the linear relationship is more clear

# take a look and compare
# before
diamonds %>% 
  ggplot(aes(carat, price)) +
  geom_hex(bins = 50)

diamonds %>% 
  ggplot(aes(price)) + 
  geom_histogram(bins = 15) 

diamonds %>% 
  ggplot(aes(carat)) + 
  geom_histogram(bins = 15) 



# after
diamonds2 %>% 
  ggplot(aes(carat_log, price_log)) +
  geom_hex(bins = 50)


diamonds2 %>% 
  ggplot(aes(price_log)) + 
  geom_histogram(bins = 10) 

diamonds2 %>% 
  ggplot(aes(carat_log)) + 
  geom_histogram(bins = 10) 


# The log-transformation is particularly useful here because it makes 
# the pattern linear, and linear patterns are the easiest to work with. 
# Let’s take the next step and remove that strong linear pattern. 
# We first make the pattern explicit by fitting a model:


## 1. model ---------------------------------------------------------------

# make the pattern explicit!
mod_diamond <- lm(price_log ~ carat_log, data = diamonds2)


# grid first!
# grid contains 4 variables (carat, carat_log, price_log, price)
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(carat_log = log2(carat)) %>% 
  add_predictions(mod_diamond, "price_log") %>% 
  # undo the log-transformation to overlay the predictions
  # on the raw data
  mutate(price = 2 ^ price_log)

# visualize
diamonds2 %>% 
  ggplot(aes(carat, price)) + 
    geom_hex(bins = 50) + 
    # take the model predictions!
    geom_line(data = grid, colour = "red", size = 1)

# the large diamonds (big carat-number) are cheaper than expected


# Did we removed the linear pattern?
# Check the residuals!
# NOTE: the residuals are the part of the data which cannot be
# explained by the model itself!

# add resididuals to the data set
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "resid_log")

# visualize => there is no linear pattern visible...
# residuals shoud spread equally (homoskedasticity)
diamonds2 %>% 
  ggplot(aes(carat_log, resid_log)) +
  geom_hex(bins = 50)

# we have successfully removed the linear pattern


# using the residuals to re-do the bar plots
# NOTE: we have removed the influence of carat 
# as a confounding variable now we see the "clean" 
# relationship between the attributes of the 
# diamond and its relative price!

diamonds2 %>% 
  ggplot(aes(cut, resid_log)) +
  geom_boxplot()


diamonds2 %>% 
  ggplot(aes(color, resid_log)) +
  geom_boxplot()


diamonds2 %>% 
  ggplot(aes(clarity, resid_log)) +
  geom_boxplot()

# Now we see the relationship we expect: as the quality of the 
# diamond increases, so too does its relative price. To interpret 
# the y axis, we need to think about what the residuals are telling 
# us, and what scale they are on:
# A residual of -1 indicates that price_log was 1 unit lower than a 
# prediction based solely on its weight. 2^−1 is 1/2, points with a value 
# of -1 are half the expected price (without taking the weight into account, 
# we overestimated the price). 
# And residuals with value 1 (2^1 = 2) are twice the predicted price 
# (without taking the weight into account, we underestimated the price).




## a more complex model ---------------------------------------------------

# we could continue to build up our model, moving the effects we’ve observed 
# into the model to make them explicit.
# We could include color, cut, and clarity into the model so that
# we also make explicit the effect of these three categorical variables

mod_diamonds2 <- lm(price_log ~ carat_log + color + cut + clarity, data = diamonds2)

# this model now includes 4 predictors and is thus getting harder to visualize
# Fortunately, they’re currently all independent which means that we can plot 
# them individually in four plots.
# NOTE: the residuals of this model are now independent of the effects of the 4
# variables (residuals are the part of the model, which are not explained through
# the model itself...)

# what is the effect of the variables all other things being equal (ceteris paribus(!))?

## 1. plot (cut)
# create the grid first
grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamonds2) %>% 
  add_predictions(mod_diamonds2)

grid %>% 
  ggplot(aes(cut, pred)) +
  geom_point()


## 2. plot (color)
# create the grid first
grid <- diamonds2 %>% 
  data_grid(color, .model = mod_diamonds2) %>% 
  add_predictions(mod_diamonds2)

grid %>% 
  ggplot(aes(color, pred)) +
  geom_point()


## 3. plot (clarity)
# create the grid first
grid <- diamonds2 %>% 
  data_grid(clarity, .model = mod_diamonds2) %>% 
  add_predictions(mod_diamonds2)

grid %>% 
  ggplot(aes(clarity, pred)) +
  geom_point()


## 4. plot (carat_log)
# create the grid first
grid <- diamonds2 %>% 
  data_grid(carat_log, .model = mod_diamonds2) %>% 
  add_predictions(mod_diamonds2)

grid %>% 
  ggplot(aes(carat_log, pred)) +
  geom_point()



# take a look at the residuals of the 2. model
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamonds2, "resid_lod2")

ggplot(diamonds2, aes(carat_log, resid_lod2)) + 
  geom_hex(bins = 50)

# there are some outliers visible...
# take a closer look
diamonds2 %>% 
  filter(abs(resid_lod2) > 1) %>% 
  add_predictions(mod_diamonds2) %>% 
  # undo the log => predicted price
  mutate(pred = round(2 ^ pred),
         pred_inaccuracy = price/pred) %>% 
  select(price, pred, pred_inaccuracy, carat:table, x:z) %>% 
  arrange(price) %>% 
  View()
