

################################# Tidy Modeling with R ################################# 


################################################################
### SOURCE: Max Kuhn and Julia Silge (https://www.tmwr.org/) ###
################################################################


# Chapter 3 - A Review of R Modeling Fundamentals -------------------------

## load packages and data -------------------------------------------------
# package
library(tidyverse)

# data
data(crickets,
     package = "modeldata")

# set ggplot-theme
theme_set(theme_minimal())

# take a first look
dim(crickets)
glimpse(crickets)

crickets %>% 
  ggplot(aes(temp, rate)) + 
  geom_point(aes(color = species),
             size = 2) + 
  geom_smooth(aes(color = species),
              method = "lm",
              se = FALSE) + 
  labs(x = "Temperature (C)",
       y = "Chirp rate (per minute)") + 
  scale_x_continuous(breaks = seq(round(min(crickets$temp), 0), max(crickets$temp) + 1, 1))



## fit a linear model -----------------------------------------------------
### 2-way interaction (several formulas are possible - all mean the same)
interaction_fit <- lm(rate ~ (temp +  species)^2,
                      data = crickets)
interaction_fit_2 <- lm(rate ~ temp + species + temp:species,
                        data = crickets)
interaction_fit_3 <- lm(rate ~ temp * species,
                        data = crickets)


summary(interaction_fit)
par(mfrow = c(2, 2))
plot(interaction_fit)

### is the 2-way interaction necessary?
main_effect_fit <- lm(rate ~ temp + species,
                      data = crickets)

anova(main_effect_fit, interaction_fit)
# interaction term is not necessary...

### main effects only
# reasses theoretical assumptions
plot(main_effect_fit)
# looks still good

summary(main_effect_fit)

### predictions
new_values <- tibble(
  species = "O. exclamationis",
  temp = 15:20
)

predict(main_effect_fit,
        new_values)



## missing values in R ----------------------------------------------------
new_values$temp[1] <- NA
new_values

# default
predict(main_effect_fit,
        new_values)

# option 1
predict(main_effect_fit,
        new_values,
        na.action = na.fail)

# option 2
predict(main_effect_fit,
        new_values,
        na.action = na.omit)



## broom-package ----------------------------------------------------------
library(broom)
corr_res <- map(mtcars %>% 
                  select(-mpg),
                cor.test,
                y = mtcars$mpg)
corr_res[[1]]
tidy(corr_res[[1]])

corr_res %>% 
  map_dfr(tidy, 
          .id = "predictor") %>% 
  mutate(predictor = fct_reorder(predictor, estimate)) %>% 
  ggplot(aes(predictor, estimate))  +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0.1) +
  geom_hline(yintercept = 0,
             color = "red") +
  labs(title = "mtcars - Correlations (and 95% confidence intervals)",
       x = "",
       y = "Correlation with mpg") +
  coord_flip()



## combining base R and tidyverse -----------------------------------------
split_by_species <- crickets %>% 
  group_nest(species)

### create a model for each species separately
model_by_species <- split_by_species %>% 
  mutate(model = map(data, ~lm(rate ~ temp, 
                               data = .x)))

model_by_species %>% 
  mutate(coef = map(model, tidy)) %>% 
  select(species, coef) %>% 
  unnest(cols = coef)



## clean up and restart R -------------------------------------------------
rm(list = ls())
.rs.restartR()





# Chapter 4 - The Ames Housing Data ---------------------------------------

## load packages and data -------------------------------------------------
# packages
library(tidyverse)
library(tidymodels)

# data
data(ames,
     package = "modeldata")

# set ggplot-theme
theme_set(theme_minimal())


## EDA --------------------------------------------------------------------
ames %>% 
  ggplot(aes(Sale_Price)) + 
  geom_histogram(bins = 50,
                 color = "white") +
  scale_x_continuous(labels = scales::dollar_format()) + 
  labs(title = "The Ames Housing Data - Sale Price",
       x = "Sale Price",
       y = "Count")


ames %>% 
  ggplot(aes(Sale_Price)) + 
  geom_histogram(bins = 50,
                 color = "white") +
  scale_x_log10(labels = scales::dollar_format()) + 
  labs(title = "The Ames Housing Data - Sale Price",
       x = "Sale Price",
       y = "Count")



## log-transformation of Sale_Price ---------------------------------------
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  janitor::clean_names()


ames %>% 
  ggplot(aes(longitude, latitude)) + 
  geom_point(aes(color = neighborhood,
                 alpha = sale_price)) + 
  theme_void() +
  theme(legend.position = "bottom") 





# Chapter 5 - Spending our Data -------------------------------------------

## training and testing set -----------------------------------------------
set.seed(501)
ames_split <- initial_split(ames,
                            prop = 0.8)

# take a first look
ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

ames_train
ames_test

# sale_price is right-skewed...
# so we need to use a stratified sample
ames %>% 
  ggplot(aes(sale_price)) + 
  geom_density() + 
  geom_vline(xintercept = c(quantile(ames$sale_price, 
                                     probs = c(0.25, 0.5, 0.75))),
             lty = 2) +
  labs(x = "Sale Price (log-10 USD)",
       y = "")

set.seed(502)
ames_split <- initial_split(ames,
                            prop = 0.8,
                            strata = sale_price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)






# Chapter 6 - Fitting Models with parsnip ---------------------------------

### set engine
lm_model <- linear_reg() %>% 
  set_engine("lm")


### fit the model (2 possible ways)
lm_form_fit <- lm_model %>% 
  fit(sale_price ~ longitude + latitude,
      data = ames_train)

lm_xy_fit <- lm_model %>% 
  fit_xy(
    x = ames_train %>% select(longitude, latitude),
    y = ames_train %>% pull(sale_price)
  )

lm_form_fit
lm_xy_fit


### extract the model output (again 2 possible ways)
# NOTE: better use extract_fit_engine()
lm_form_fit$fit %>% 
  summary()
lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

lm_xy_fit$fit %>% 
  summary()
lm_xy_fit %>% 
  extract_fit_engine() %>% 
  summary()

# broom-package
tidy(lm_form_fit)


### predictions
ames_test_small <- ames_test %>% 
  slice(1:5)

predict(lm_form_fit,
        new_data = ames_test_small)

ames_test_small %>% 
  select(sale_price) %>% 
  bind_cols(predict(lm_form_fit,
                    ames_test_small)) %>% 
  # add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit,
                    ames_test_small,
                    type = "pred_int"))


## fit a tree-model
tree_model <- decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- tree_model %>% 
  fit(sale_price ~ longitude + latitude,
      data = ames_test)

ames_test_small %>% 
  select(sale_price) %>% 
  bind_cols(predict(tree_fit,
                    ames_test_small))


## parsnip helper function
parsnip_addin()





# Chapter 7 - A Model Workflow --------------------------------------------

lm_model <- linear_reg() %>% 
  set_engine("lm")


# base version
lm_workflow <- workflow() %>% 
  add_model(lm_model)

lm_workflow

# add a simple preprocessor
lm_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_formula(sale_price ~ longitude + latitude)

lm_workflow


### fit the model
lm_fit <- fit(lm_workflow,
              ames_train)

lm_fit


### predictions
predict(lm_fit,
        ames_test %>% 
          slice(1:3))


### update preprocessor (i.e. formula)
lm_fit %>% 
  update_formula(sale_price ~ longitude)

lm_workflow <- lm_workflow %>% 
  remove_formula() %>% 
  add_variables(outcome = sale_price,
                predictors = c(longitude, latitude))

lm_workflow


## multilevel models ------------------------------------------------------
# model-specific formula is needed
library(multilevelmod)
library(lme4)
library(nlme)


multilevel_spec <- linear_reg() %>% 
  set_engine("lmer")

multilevel_workflow <- workflow() %>% 
  add_variables(outcome = distance, 
                predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, 
            # this model-specific formula is given to the model
            formula = distance ~ Sex + (age | Subject))

multilevel_fit <- fit(multilevel_workflow,
                      data = Orthodont)
multilevel_fit



## survival analysis  -----------------------------------------------------
# model-specific formula is needed
library(censored)

parametric_spec <- survival_reg()

parametric_workflow <- workflow() %>% 
  add_variables(outcome = c(fustat, futime),
                predictors = c(age, rx)) %>% 
  add_model(parametric_spec,
            # this model-specific formula is given to the model
            formula = Surv(futime, fustat) ~ age +  strata(rx))

parametric_fit <- fit(parametric_workflow,
                      data = ovarian)
parametric_fit


## multiple workflows at once ---------------------------------------------
library(workflowsets)

# create a set of formulas
location <- list(
  longitude = sale_price ~ longitude,
  latitude = sale_price ~ latitude,
  coords = sale_price ~ longitude + latitude,
  neighborhood = sale_price ~ neighborhood
)

# create workflow set (we could also provide a list with multiple 
# models...)
location_models <- workflow_set(preproc = location,
                                models = list(lm = lm_model))

# extract workflows
location_models
location_models %>% 
  extract_workflow(id = "coords_lm")

# fit the models
location_models <- location_models %>% 
  mutate(fit = map(info, ~ fit(.x$workflow[[1]],
                               ames_train)))

location_models$fit[[1]]
location_models$fit[[3]]


## evaluating the test set ------------------------------------------------
lm_workflow

# last_fit takes a data split as input
final_lm_res <- last_fit(lm_workflow,
                         ames_split)
final_lm_res

fitted_lm_workflow <- extract_workflow(final_lm_res)
fitted_lm_workflow

collect_metrics(final_lm_res)
collect_predictions(final_lm_res)





# CLEAN UP FIRST ----------------------------------------------------------
rm(list = ls())
.rs.restartR()

### code needed for the next chapter
library(tidyverse)
library(tidymodels)

theme_set(theme_minimal())

data(ames)

# For simple transformations of the outcome column(s), we strongly 
# suggest that those operations be conducted outside of the recipe
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  janitor::clean_names()

set.seed(502)
ames_split <- initial_split(ames, 
                            prop = 0.80, 
                            strata = sale_price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = sale_price, 
                predictors = c(longitude, latitude))

lm_fit <- fit(lm_wflow, ames_train)





# Chapter 8 - Feature Engineering with recipes ----------------------------

### using a recipe
simple_ames <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type,
                      data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_dummy(all_nominal_predictors())

simple_ames

# add to an existing workflow
lm_wflow <- lm_wflow %>% 
  # need to remove the existing one first...
  remove_variables() %>% 
  add_recipe(simple_ames)


### fit the model
lm_fit <- fit(lm_wflow,
              ames_train)
lm_fit

### predictions
predict(lm_fit, ames_test %>% 
          slice(1:3))


### extract the recipe or the results
lm_fit %>% 
  extract_recipe()

lm_fit %>% 
  extract_fit_engine() %>% 
  summary()

lm_fit %>% 
  extract_fit_engine() %>% 
  tidy() %>% 
  filter(term != "(Intercept)") %>% 
  # one term is NA (?)
  # for example: there is no data available for Hayden_Lake
  # see step_other(...) below
  na.omit() %>% 
  mutate(term = str_replace_all(term, "_", " "),
         term = str_replace_all(term, "neighborhood", "Neighborhood:"),
         term = str_replace_all(term, "bldg type", "Type of Building:"),
         term = str_replace(term, "gr liv area", "Living Area"),
         term = str_replace(term, "year built", "Year Built"),
         term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - (1.96 * std.error),
                    ymax = estimate + (1.96 * std.error)),
                width = 0.1) + 
  geom_hline(yintercept = 0,
             color = "red") + 
  labs(x = "",
       y = "Estimate") + 
  coord_flip()


### use step_*-functions from recipe
simple_ames <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type,
                      data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood,
             threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())



## interaction terms ------------------------------------------------------
ames_train %>% 
  ggplot(aes(gr_liv_area, 10^sale_price)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() + 
  scale_y_log10(labels = scales::dollar_format()) + 
  facet_wrap(~bldg_type) + 
  labs(x = "Living Area",
       y = "Sale Price (USD)")


simple_ames <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type,
                      data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood,
             threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_"))

# take a look at the processed data
simple_ames %>% 
  prep() %>% 
  juice() %>% 
  View()


## spline functions -------------------------------------------------------
library(patchwork)
library(splines)

plot_splines_lat <- function(n_splines) {
  
  ames_train %>% 
    ggplot(aes(latitude, 10^sale_price)) + 
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm",
                formula = y ~ ns(x, df = n_splines),
                se = FALSE) + 
    scale_y_log10(labels = scales::dollar_format()) + 
    labs(title = glue::glue("{n_splines} Spline Terms"),
         subtitle = "Latitude",
         x = "Latitude",
         y = "Sale Price (USD)")
  
}

plot_splines_long <- function(n_splines) {
  
  ames_train %>% 
    ggplot(aes(longitude, 10^sale_price)) + 
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm",
                formula = y ~ ns(x, df = n_splines),
                se = FALSE) + 
    scale_y_log10(labels = scales::dollar_format()) + 
    labs(title = glue::glue("{n_splines} Spline Terms"),
         subtitle = "Longitude",
         x = "Longitude",
         y = "Sale Price (USD)")
  
}

# NOTE: under- or overfitting in some cases
# n_splines between 5 and 25 seems reasonable
( plot_splines_lat(2) + plot_splines_lat(5) ) / 
  ( plot_splines_lat(25) + plot_splines_lat(50) ) /
  ( plot_splines_lat(75) + plot_splines_lat(100) ) /
  ( plot_splines_lat(250) + plot_splines_lat(1000) ) 


( plot_splines_long(2) + plot_splines_long(5) ) / 
  ( plot_splines_long(25) + plot_splines_long(50) ) /
  ( plot_splines_long(75) + plot_splines_long(100) ) /
  ( plot_splines_long(250) + plot_splines_long(1000) ) 


simple_ames <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type + latitude + longitude,
                      data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_")) %>% 
  step_ns(latitude, 
          deg_free = 20) %>% 
  step_ns(longitude, 
          deg_free = 20)

# take a look
simple_ames %>% 
  prep() %>% 
  juice() %>% 
  View()
# NOTE: need to determine if both neighborhood and latitude should be in the model 
# since they both represent the same underlying data in different ways...


## PCA --------------------------------------------------------------------
# several predictors measure the size. Maybe it is a good idea to 
# condense these predictors with a PCA (here: form 5 similar predictors,
# which are correlated with each other to 2 uncorrelated predictors)
# NOTE: step_pca assumes that all predictors are on the same scale!
# This is the case here. But it might be necessary to use step_normalize() 
# first...

simple_ames <- recipe(sale_price ~ neighborhood + gr_liv_area + total_bsmt_sf + open_porch_sf + 
                        first_flr_sf + second_flr_sf + year_built + bldg_type,
                      data = ames_train) %>% 
  step_log(gr_liv_area) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_pca(matches("(sf$)|(gr_liv)"),
           num_comp = 2)

simple_ames %>% 
  prep() %>% 
  juice() %>% 
  glimpse()


## tidy a recipe ----------------------------------------------------------
ames_rec <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + 
                     bldg_type + latitude + longitude,
                   data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood,
             threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_")) %>% 
  step_ns(latitude, longitude, deg_free = 20)

# we can also name the id
ames_rec %>% 
  tidy()

ames_rec <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + 
                     bldg_type + latitude + longitude,
                   data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood,
             threshold = 0.01,
             id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_")) %>% 
  step_ns(latitude, longitude, deg_free = 20)

ames_rec %>% 
  tidy()


### let's fit the model with this new recipe
lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)

lm_fit %>% 
  extract_recipe(estimated = TRUE) %>% 
  tidy(id = "my_id") %>% 
  print(n = nrow(.))


lm_fit %>% 
  extract_fit_engine() %>% 
  tidy() %>% 
  filter(abs(p.value) <= 0.05) %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(term, estimate)) + 
  geom_col(aes(fill = estimate > 0),
           show.legend = FALSE) + 
  coord_flip() +
  labs(title = "Ames Housing Data - Significant Predictors only",
       x = "",
       y = "Estimate")





# lets do some clean up before we continue... -----------------------------
rm(list = ls())
.rs.restartR()

### code needed for the next chapter
library(tidyverse)
library(tidymodels)

theme_set(theme_minimal())

data(ames)

# For simple transformations of the outcome column(s), we strongly 
# suggest that those operations be conducted outside of the recipe
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  janitor::clean_names()

set.seed(502)
ames_split <- initial_split(ames, 
                            prop = 0.80, 
                            strata = sale_price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_variables(outcome = sale_price, 
                predictors = c(longitude, latitude))

lm_fit <- fit(lm_wflow, ames_train)





# CLEAN UP FIRST ----------------------------------------------------------
rm(list = ls())
.rs.restartR()

### code needed for the next chapter
library(tidyverse)
library(tidymodels)

theme_set(theme_minimal())

# load data
data(ames)

### 1. For simple transformations of the outcome column(s), we strongly 
# suggest that those operations be conducted outside of the recipe
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  janitor::clean_names()

### 2. create training and testing set
set.seed(502)
ames_split <- initial_split(ames, 
                            prop = 0.80, 
                            strata = sale_price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

### 3. !!!EDA!!! (on the training set only)

### 4. create model
lm_model <- linear_reg() %>% 
  set_engine("lm")

### 5. create a recipe
ames_rec <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type +
                     latitude + longitude,
                   data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood, 
             threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_")) %>% 
  step_ns(latitude, longitude,
          deg_free = 20)

# take a look
ames_rec %>% 
  prep() %>% 
  juice()

### 6. create workflow
lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

### 7. fit the model (on the training set)
lm_fit <- fit(lm_wflow,
              ames_train)





# Chapter 9 - Judging Model Effectiveness ---------------------------------


## Regression metrics (mainly 2) ------------------------------------------
# 1. Root mean squared error (RMSE) => measures accuracy 
#    relatively uniform accuracy across the range of the outcome
# 2. Coefficient of determination (R^2) => measures correlation
#    tighter correlation between the observed and predicted values 
#    but the model may perform poorly in the tails

# we use the test data here just for illustrative purposes (normally
# you shoud use the test set only once(!))
ames_test_res <- predict(lm_fit,
                         new_data = ames_test %>% 
                           select(-sale_price))
ames_test_res

# let's combine the predicted values with the observed ones
ames_test_res <- ames_test_res %>% 
  bind_cols(ames_test %>% 
              select(sale_price))
ames_test_res

# plot
ames_test_res %>% 
  ggplot(aes(sale_price, .pred)) + 
  geom_point() + 
  geom_abline(lty = 2) + 
  # same scale and size for both axes
  coord_obs_pred()

# calculate RMSE
rmse(ames_test_res,
     truth = sale_price,
     estimate = .pred)

# create a metric set
ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res,
             truth = sale_price,
             estimate = .pred)


## Binary classification metrics ------------------------------------------
data("two_class_example")
tibble(two_class_example)
# NOTE: "Class1" and "Class2" are the predicted probabilities,
#       "predicted" are the discrete predictions

# confusion matrix
conf_mat(two_class_example,
         truth = truth = truth,
         estimate = predicted)

# accuracy
accuracy(two_class_example,
         truth = truth,
         estimate = predicted)

# Matthews correlation coefficient
# (summarizes the confusion matrix: emphasizes pos. & neg. class)
mcc(two_class_example,
    truth = truth,
    estimate = predicted)

# F1 metric 
# (summarizes the confusion matrix: emphasizes pos. class)
f_meas(two_class_example,
       truth = truth,
       estimate = predicted)

# crete metric set
calssification_set <- metric_set(accuracy, mcc, f_meas)
calssification_set(two_class_example,
                   truth = truth,
                   estimate = predicted)

# change the event level (level of interest)
# default: first level
f_meas(two_class_example,
       truth = truth,
       estimate = predicted,
       event_level = "second")


# receiver operating characteristic (ROC)
two_class_curve <- roc_curve(two_class_example,
                             truth,
                             Class1)

autoplot(two_class_curve)
# OR
roc_value <- roc_auc(two_class_example,
                     truth,
                     Class1) %>% 
  pull(.estimate)

two_class_curve %>% 
  ggplot(aes(1-specificity, sensitivity)) + 
  geom_path(color = "red",
            size = 1.5) + 
  geom_abline(lty = 2) + 
  geom_label(x = 0.25,
             y = 0.65,
             label = glue::glue("ROC:\n{round(roc_value, 2)}\n~{round(roc_value * 100,2)}%"))


## Multiclass classification metrics --------------------------------------
data("hpc_cv")
tibble(hpc_cv)
# NOTE: obs = actual values
#       pred = discrete predictions
#       VF:L = probabilities for each class

# mostly the same syntax can be used as above...

# confusion matrix
conf_mat(hpc_cv,
         truth = obs,
         estimate = pred)

# accuracy
accuracy(hpc_cv, 
         truth = obs, 
         estimate = pred)

# etc.

# metric set
multilevel_set <- metric_set(accuracy, mcc, f_meas)
multilevel_set(hpc_cv,
               truth = obs,
               estimate = pred)


# receiver operating characteristic (ROC)
# NOTE: all classes must be provided
roc_auc(hpc_cv, 
        obs, 
        VF, F, M, L)
# you can set several weighting-options with the estimator-argument
roc_auc(hpc_cv, 
        obs, 
        VF, F, M, L,
        estimator = "macro_weighted")

# we can also group them (here by the folds of the resample)
hpc_cv %>% 
  group_by(Resample) %>% 
  roc_curve(obs, 
            VF, F, M, L) %>% 
  autoplot()
# OR
resamples <- c(paste0("Fold0", 1:9), "Fold10")
multiclass_curve <- tibble(
  resample = vector(mode = "character"),
  .level = vector(mode = "character"),
  .threshold = vector(mode = "numeric"),
  specificity = vector(mode = "numeric"),
  sensitivity = vector(mode = "numeric")
)

# loop trough to get information for ROC-curve
for(i in seq_along(resamples)){
  
  temp <- hpc_cv %>% 
    filter(Resample == resamples[i])
  
  temp <- temp %>% 
    roc_curve(obs, 
              VF, F, M, L) %>% 
    mutate(resample = resamples[i]) %>% 
    relocate(resample)
  
  multiclass_curve <- bind_rows(multiclass_curve, temp)
  rm(temp)
  
}

multiclass_curve

multiclass_curve %>% 
  ggplot(aes(1-specificity, sensitivity)) + 
  geom_path(aes(color = resample),
            size = 1.5) + 
  geom_abline(lty = 2) + 
  facet_wrap(~ .level) + 
  labs(color = "Resample:")
