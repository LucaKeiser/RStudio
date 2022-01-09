
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


# Why are low qualitiy diamonds more expensive? ---------------------------

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



## modeling ---------------------------------------------------------------

### A first model ---------------------------------------------------------

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


### 1. model --------------------------------------------------------------

# make the pattern explicit!
mod_diamond <- lm(price_log ~ carat_log, data = diamonds2)
summary(mod_diamond)

# grid first!
# grid contains 4 variables (carat, carat_log, price_log, price)
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(carat_log = log2(carat)) %>% 
  add_predictions(mod_diamond, "price_log") %>% 
  # undo the log-transformation to overlay the predictions
  # on the raw data
  mutate(price = 2 ^ price_log) %>% 
  relocate(price, price_log, carat, carat_log)

# visualize
diamonds2 %>% 
  ggplot(aes(carat, price)) + 
    geom_hex(bins = 50) + 
    # take the model predictions!
    geom_line(data = grid, colour = "red", size = 1)

# large diamonds (big carat-number) are cheaper than expected


# Did we removed the linear pattern?
# Check the residuals!
# NOTE: the residuals are the part of the data which cannot be
# explained by the model itself!

# add residuals to the data set
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
# prediction based solely on its weight. 2^−1 is 1/2. So, points with a value 
# of -1 are half the expected price (without taking the weight into account, 
# we overestimated the price of this points). 
# And residuals with value 1 (2^1 = 2) are twice the predicted price 
# (without taking the weight into account, we underestimated the price
# of this points).




### a more complex model --------------------------------------------------

# we could continue to build up our model, moving the effects we’ve observed 
# into the model to make them explicit.
# We could include color, cut, and clarity into the model so that
# we also make explicit the effect of these three categorical variables

mod_diamonds2 <- lm(price_log ~ carat_log + color + cut + clarity, data = diamonds2)
summary(mod_diamonds2)

# this model now includes 4 predictors (3 of them are categorical variables with multiple levels!
# => they are represented as factors  here...) and is thus getting harder to visualize.
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
  add_residuals(mod_diamonds2, "resid_log")

ggplot(diamonds2, aes(carat_log, resid_log)) + 
  geom_hex(bins = 50)

# there are some outliers visible...
# take a closer look
diamonds2 %>% 
  filter(abs(resid_log) > 1) %>% 
  add_predictions(mod_diamonds2) %>% 
  # undo the log => predicted price
  mutate(pred = round(2 ^ pred),
         pred_inaccuracy = price/pred) %>% 
  select(price, pred, pred_inaccuracy, carat:table, x:z) %>% 
  arrange(price) %>% 
  View()



## Exercises --------------------------------------------------------------

# 1)
# In the plot of carat_log vs. price_log, there are some bright vertical strips. What do they represent?

diamonds2 %>% 
  ggplot(aes(carat_log, price_log)) +
  geom_point(alpha = 0.1)

diamonds2 %>% 
  ggplot(aes(carat, price)) +
  geom_point(alpha = 0.1)

# The distribution of diamonds has more diamonds at round or otherwise human-friendly numbers (fractions).



# 2)
# If log(price) = a_0 + a_1 * log(carat), what does that say about the relationship between price and carat?

mod_diamond <- lm(price_log ~ carat_log, data = diamonds2)
summary(mod_diamond)

grid <- diamonds2 %>% 
  data_grid(carat = seq(0, 5, by = 0.25)) %>%
  # need to add carat_log!
  mutate(carat_log = log2(carat)) %>% 
  add_predictions(model = mod_diamond, "pred_log") %>% 
  mutate(pred = 2^pred_log) %>% 
  relocate(carat, carat_log, pred, pred_log)
  

grid %>% 
  ggplot(aes(x = carat, y = pred)) + 
  geom_line()

# the relationship is not linear...

# if you double the carat number, the price increases by factor 3.2
# 1.
grid %>% 
  filter(carat == 1 | carat == 2) %>% 
  summarise(ratio = pred[2] / pred[1])

# 2.
# 2^ is needed because the model uses log2()-values!
2^coef(mod_diamond)[2]



######################### SIDE NOTE #########################

# interpretation of coefficients

## 1. linear-linear
# a 1 unit change in x is associated with a β change in y.

## 2. linear-log
# a 1% percentage change in x is associated with a 0.01*β-percent change in y.

## 3. log-linear
# a 1 unit change in x is associated with a 100*β-percent change in y.

## 4. log-log
# a 1% percentage change in x is associated with a β-percent change in y.

#############################################################



# 3)
# Extract the diamonds that have very high and very low residuals. 
# Is there anything unusual about these diamonds? Are they particularly 
# bad or good, or do you think these are pricing errors?

# remember the data/model again

# mod_diamonds2 <- lm(price_log ~ carat_log + color + cut + clarity, data = diamonds2)
# summary(mod_diamonds2)
# 
# diamonds2 <- diamonds %>% 
#   # take care of outliers
#   filter(carat <= 2.5) %>% 
#   
#   # transform the variables 
#   # => makes the relationship linear
#   # => and the distribution symmetrical
#   mutate(price_log = log2(price),
#          carat_log = log2(carat)) %>% 
#   
#   # add predictions and residuals
#   add_predictions(mod_diamonds2, "pred_log") %>% 
#   add_residuals(mod_diamonds2, "resid_log") %>% 
#   
#   # transform
#   mutate(pred = 2^pred_log) %>%
#   mutate(resid = 2^resid_log)
# 
# 

diamonds2 %>% 
  filter(abs(resid_log) > 1) %>%
  # add predictions
  add_predictions(mod_diamonds2, "pred_log") %>% 
  mutate(pred = 2^pred_log) %>% 
  select(price, price_log, pred, pred_log, carat, carat_log, cut, color, clarity, depth) %>% 
  arrange(price)





# 4)
# Does the final model, mod_diamonds2, do a good job of predicting diamond prices? 
# Would you trust it to tell you how much to spend if you were buying a diamond?


# plot the resiudals of the model
diamonds2 %>% 
  ggplot(aes(carat_log, resid_log)) + 
  geom_hex(bins = 50) + 
  geom_hline(yintercept = 0,
             lty = 2, 
             size = 2,
             color = "red")

mod_diamonds2_summary <- diamonds2 %>% 
  summarise(
    # root mean squared error
    rmse = sqrt(mean(resid_log^2)),
    # mean absolute error
    mae = mean(abs(resid_log)),
    # 95% of the residuals are within this range
    p025 = quantile(resid_log, 0.025),
    p975 = quantile(resid_log, 0.975))

mod_diamonds2_summary

summary(mod_diamond)
summary(mod_diamonds2)












# What affects the number of daily flights? -------------------------------

nycflights13::flights

# flights leaving NYC per day
daily <- nycflights13::flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

daily %>% 
  ggplot(aes(date, n)) +
  geom_line(group = 1) +
  geom_hline(yintercept = mean(daily$n),
             lty = 2,
             size = 2,
             color = "red") +
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b")


## Day of week ------------------------------------------------------------

# There is a very strong day-of-week effect that dominates the subtler patterns.
# Let's start by looking at the distribution of flight numbers by day-of-week.

daily <- daily %>% 
  # get the weekdays!
  mutate(wday = wday(date, label = TRUE)) %>% 
  # relevel
  mutate(wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

daily %>% 
  ggplot(aes(wday, n)) +
  geom_boxplot()

# fewer flights on the weekends => business flights are during the week
# strong day-of-week effect
# we can remove this effect by fitting a model and control for the weekdays.

mod <- lm(n ~ wday, data = daily)
mod

# a grid with just 7 observations (1 prediciton per weekday!)
grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

# overlay the predictions on the raw data
daily %>% 
  ggplot(aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, 
             color = "red",
             size = 3)
  


# take a look at the residuals
daily <- daily %>% 
  add_residuals(mod)

daily %>% 
  ggplot(aes(date, resid)) +
  geom_line() + 
  geom_hline(yintercept = 0,
             lty = 2,
             color = "red") +
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b")

# Note the change in the y-axis: now we are seeing the deviation from the expected number 
# of flights, given the day of week (!). This plot is useful because now that we’ve removed much 
# of the large day-of-week effect, we can see some of the subtler patterns that remain:


# 1)
# Our model seems to fail starting in June (big residuals): you can still see a strong regular pattern 
# that our model hasn’t captured. Drawing a plot with one line for each day of the week 
# makes the cause easier to see:

daily %>% 
  # filter(wday == "Sat") %>% 
  ggplot(aes(date, resid, color = wday)) + 
  geom_line() + 
  geom_hline(yintercept = 0,
             lty = 2, 
             color = "red") +
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b")

# our model fails to predict the number of flights especially on Saturdays 
# (during summer too high, during fall too low...)


# 2)
# take a look at big residuals
daily %>% 
  filter(abs(resid) >= 100) %>% 
  arrange(resid)

# There are some days with far fewer flights than expected:
# public holidays => New Year's Day, Thanksgiving, Christmas


# 3)
# there seems to be a smoother long term trend over the course of the year
daily %>% 
  ggplot(aes(date, resid)) +
  geom_line() + 
  geom_smooth(se = FALSE,
              span = 0.2) + 
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b")

# threre are fewer flights in January (and December), and more in summer (May-Sep). We cannot do much with
# this pattern quantitatively beacuse we do not have enough observations...



## Seasonal Saturday effect -----------------------------------------------

# Take a closer look at Saturdays only

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_line() + 
  geom_point() +
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b") +
  geom_hline(yintercept = mean(daily[daily$wday == "Sat", ]$n),
             lty = 2,
             color  = "red")

# (We’ve used both points and lines to make it more clear what is data and what is interpolation.)

# hypothesis: Summer holidays (School) => more flights during the summer 
#             Winter holidays => less common to go away during this time...


# Lets create a “term” variable that roughly captures the three school terms, and check our work with a plot:

# we split up the year into spring, summer and fall
term <- function(date) {
  # convert numeric to factor with base::cut
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
      )
  }


daily <- daily %>% 
  mutate(term = term(date)) 


daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, color = term)) +
  geom_line() +
  geom_point() +
  scale_x_date(NULL, 
               date_breaks = "1 month", 
               date_labels = "%b") 



# how does this new variable affect the other days of the week?
daily %>% 
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()

# looks like the term has a significant effect of the number of flights
# so fitting a separate day of week effect for each term is reasonable
# this improves the model but not as much as we might hope...

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, color = model)) + 
  geom_line(alpha = 0.75)



# we can see the problem by overlaying the predictions from the model on the raw data
grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")


daily %>% 
  ggplot(aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") + 
  facet_wrap(~ term)

plot(mod2)

# Our model is finding the mean effect, but we have a lot of big outliers, so mean 
# tends to be far away from the typical value. We can alleviate this problem by using 
# a model that is robust to the effect of outliers: MASS::rlm(). This greatly reduces 
# the impact of the outliers on our estimates, and gives a model that does a good job of 
# removing the day of week pattern:


mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) +
  geom_line() +
  geom_hline(yintercept = 0,
             lty = 2, 
             color  = "red")

# compare to the other models
daily_mod <- daily %>% 
  gather_residuals(without_term = mod1,
                   with_term = mod2,
                   with_term_MASS = mod3)
  
ggplot() +
  geom_line(data = filter(daily_mod, model != "with_term_MASS"),
            aes(date, resid, color = model), 
            alpha = 0.75) +
  geom_line(data = filter(daily_mod, model == "with_term_MASS"),
            aes(date, resid), color = "black") +
  geom_hline(yintercept = 0,
             lty = 2, 
             color = "black",
             size = 2) +
  theme(legend.position = "none")









