
################################# Tidy Modeling with R ################################# 


################################################################
### SOURCE: Max Kuhn and Julia Silge (https://www.tmwr.org/) ###
################################################################


# Chapter 10 - Resampling for Evaluating Performance ----------------------

## load packages and data -------------------------------------------------
# package
library(tidyverse)
library(tidymodels)

theme_set(theme_minimal())

### data
data(crickets,
     package = "modeldata")


## code needed for this chapter --------------------------------------------
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  janitor::clean_names()


### training and testing set
set.seed(502)
ames_split <- initial_split(ames, 
                            prop = 0.8,
                            strata = sale_price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

### create model
lm_model <- linear_reg() %>% 
  set_engine("lm")


### create recipe
ames_rec <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + 
                     bldg_type + latitude + longitude,
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


### create workflow
lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)


### fit the model
lm_fit <- fit(lm_wflow,
              data = ames_train)

# take a look (TIE-fighter plot)
lm_fit %>% 
  extract_fit_engine() %>% 
  tidy(conf.int = TRUE) %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(term, estimate)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high)) + 
  geom_hline(yintercept = 0,
             color = "red",
             lty = 2) +
  coord_flip() +
  labs(x = "",
       y = "Estimate")



## create random forest model ---------------------------------------------
rf_model <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_formula(sale_price ~ neighborhood + gr_liv_area + year_built + 
                bldg_type + latitude + longitude)

rf_fit <- fit(rf_wflow, data = ames_train)


## compare the models ------------------------------------------------------
estimate_perf <- function(model, dat) {
  
  # capture the names of the 'model' and 'dat' object
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- str_replace_all(data_name, "ames_", "")
  
  print(cl)
  print(obj_name)
  
  # estimate metrics
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>% 
    predict(dat) %>% 
    bind_cols(dat %>% 
                select(sale_price)) %>% 
    reg_metrics(sale_price, .pred) %>% 
    select(-.estimator) %>%
    mutate(object = obj_name,
           data = data_name)
  
}

estimate_perf(lm_fit,
              ames_train)
estimate_perf(rf_fit, 
              ames_train)

### evaluate on the test set
estimate_perf(rf_fit,
              ames_test)

# low bias vs. high bias models!
# low bias models (here: random forest) are capable 
# of reproducing very complex relationships... But this
# can lead to predictions which are too optimistic 
# (overfitting):

# RMSE:
# object 	train 	test
# lm_fit 	0.0754 	0.0736
# rf_fit 	0.0365 	0.0704 

# => Notice that the linear regression model is consistent 
#    between training and testing, because of its limited 
#    complexity.


## cross-validation -------------------------------------------------------
## 'normal' cross-validation
set.seed(1001)
ames_folds <- vfold_cv(ames_train,
                       v = 10)

ames_folds
ames_folds$splits[[1]] %>% 
  analysis()
ames_folds$splits[[1]] %>% 
  assessment()


## repeated cross-validation
vfold_cv(ames_train, 
         v = 10,
         repeats = 5)


## leave-one-out cross-validation
# Each model predicts the single excluded data point. At the end of resampling, 
# the n predictions are pooled to produce a single performance statistic.
loo_cv(ames_train)

## Monte Carlo cross-validation (0.9/0.1 with 20 resamples)
# The difference between MCCV and regular cross-validation is that, for MCCV, 
# this proportion of the data is randomly selected each time. This results in 
# assessment sets that are not mutually exclusive. 
mc_cv(ames_train, 
      prop = 9/10, 
      times = 20)


## validation set (if a lot data is available)
set.seed(1002)
val_set <- validation_split(ames_train,
                            prop = 3/4)
val_set


## bootstrapping
# A bootstrap sample of the training set is a sample that is the same size 
# as the training set but is drawn with replacement. The sizes of the assessment 
# sets vary. Bootstrap samples produce performance estimates that have very low 
# variance (unlike cross-validation) but have significant pessimistic bias.
bootstraps(ames_train,
           times = 5)


## rolling forecasting origin resampling
# if there is a strong time component within the data (time series data)
time_slices <- tibble(x = 1:365) %>% 
  rolling_origin(initial = 6 * 30,
                 assess = 30,
                 skip = 29,
                 cumulative = FALSE)


data_range <- function(x) {
  summarize(x, 
            first = min(x),
            last = max(x)
  )
}

map_dfr(time_slices$splits,
        ~ analysis(.x) %>% 
          data_range())
map_dfr(time_slices$splits, 
        ~ assessment(.x) %>% 
          data_range())


## estimating performance -------------------------------------------------
keep_pred <- control_resamples(save_pred = TRUE,
                               save_workflow = TRUE,
                               verbose = TRUE)

start <- Sys.time()
set.seed(1003)
rf_res <- rf_wflow %>% 
  fit_resamples(resamples = ames_folds,
                control = keep_pred) 
end <- Sys.time()
end - start # Time difference of ~24 secs

rf_res
rf_res %>% 
  collect_metrics(summarize = FALSE)
rf_res %>% 
  collect_metrics()
# the performance estimates are much more realistic than before!

# assessment set predictions
assess_res <- collect_predictions(rf_res)
assess_res

assess_res %>% 
  ggplot(aes(sale_price, .pred)) + 
  geom_point(alpha = 0.15) + 
  geom_abline(color = "red") + 
  coord_obs_pred() + 
  labs(title = "Ames housing data",
       subtitle = "of-sample observed vs. predicted values",
       x = "Sale Prica (US-$ | log-10)",
       y = "Predicted value (US-$ | log-10)")
# two houses are significantly overpredcited by our model...

over_predcited <- assess_res %>% 
  mutate(residual = sale_price - .pred) %>% 
  arrange(desc(abs(residual))) %>% 
  slice(1:2)
over_predcited

ames_train %>% 
  slice(over_predcited$.row) %>% 
  glimpse()


## let's use the validation instead of the cross-validation set
val_res <- rf_wflow %>% 
  fit_resamples(resamples = val_set,
                control = keep_pred)
val_res %>% 
  collect_metrics()


## parallel processing ----------------------------------------------------
# number of physical cores
parallel::detectCores(logical = FALSE)
# number of all available cores
parallel::detectCores()

# NOTE: The numbers usable cores is limited to the number of used resamples:
#       "Since there were only five resamples, the number of cores used when 
#       parallel_over = "resamples" (= default) is limited to five."
#       You can use parallel_over = "everything" to change this behaviour. 
#       But this might have other downsides. Usually parallel_over = "resamples"
#       is a good/the better option.
#       => see chapter 13.5. Tools for Efficient Grid Search


# library(doParallel)
# cl <- makeCluster(8)
# registerDoParallel(cl)
# 
# # let's fit again
# start <- Sys.time()
# microbenchmark::microbenchmark(
#   set.seed(1003),
#   rf_res <- rf_wflow %>% 
#     fit_resamples(resamples = ames_folds,
#                   control = keep_pred) 
# )
# end <- Sys.time()
# end - start
# 
# # end cluster
# stopCluster(cl)


## saving the resampled objects -------------------------------------------
lm_model <- linear_reg() %>% 
  set_engine("lm")

ames_rec <-recipe(sale_price ~ sale_price + neighborhood + gr_liv_area + year_built + 
                    bldg_type + latitude + longitude, 
                  data = ames_train) %>% 
  step_other(neighborhood, 
             threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ gr_liv_area:starts_with("bldg_type_") ) %>% 
  step_ns(latitude, longitude, 
          deg_free = 20)

lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, data = ames_train)

extract_recipe(lm_fit)

get_model <- function(x) {
  extract_fit_parsnip(x) %>% 
    tidy()
}

# fit
ctrl <- control_resamples(extract = get_model,
                          verbose = TRUE)
lm_res <- lm_wflow %>% 
  fit_resamples(resamples = ames_folds,
                control = ctrl)

# extract coefficients
lm_res
lm_res$.extracts[[1]][[1]]

all_coef <- map_dfr(lm_res$.extracts,
                    ~ .x[[1]][[1]])

# coefficient for each resample
all_coef %>% 
  count(term, 
        sort = TRUE)

all_coef %>%
  group_by(term) %>% 
  summarise(resampled_estimate = mean(estimate),
            resampled_sd = mean(std.error))




# Chapter 11 - Comparing Models with Resampling ---------------------------
### 1
basic_rec <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built +
                      bldg_type + latitude + longitude,
                    data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood,
             threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

basic_rec %>% 
  prep() %>% 
  juice()


### 2
interaction_rec <- basic_rec %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_"))

interaction_rec %>% 
  prep() %>% 
  juice()


### 3
spline_rec <- interaction_rec %>% 
  step_ns(latitude, longitude,
          deg_free = 20)

spline_rec %>% 
  prep() %>% 
  juice()


### set up for model comparison (create preprocessing objects
#    => named list!)
preproc <- list(basic = basic_rec,
                interact = interaction_rec,
                splines = spline_rec)

lm_models <- workflow_set(preproc,
                          list(lm = linear_reg()),
                          cross = FALSE)
lm_models

lm_models <- lm_models %>% 
  workflow_map("fit_resamples",
               seed = 1101,
               verbose = TRUE,
               resamples = ames_folds,
               control = keep_pred)
lm_models

collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")


### add the random forest model and compare
four_models <- as_workflow_set(random_forest = rf_res) %>% 
  bind_rows(lm_models)

four_models
four_models %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq")

four_models %>% 
  autoplot(metric = "rsq") +
  geom_text(aes(label = wflow_id),
            vjust = -1,
            hjust = -0.1) +
  expand_limits(x = c(1, 4.5)) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "",
       y = expression(paste(R^{2}, " statistics")),
       color = "")


## comparing resampled performance statistics -----------------------------
rsq_indiv_estimates <- collect_metrics(four_models,
                                       summarize = FALSE) %>% 
  filter(.metric == "rsq")

rsq_indiv_estimates

rsq_wider <- rsq_indiv_estimates %>% 
  select(wflow_id, .estimate, id) %>% 
  pivot_wider(id_cols = "id",
              names_from = "wflow_id",
              values_from = ".estimate")
rsq_wider

rsq_wider %>% 
  select(-id) %>% 
  corrr::correlate()

rsq_indiv_estimates %>% 
  mutate(wflow_id = fct_reorder(wflow_id, .estimate)) %>% 
  ggplot(aes(x = wflow_id,
             y = .estimate,
             group = id,
             color = id)) + 
  geom_line(alpha = 0.5,
            linewidth = 1.25) +
  labs(x = "",
       y = expression(paste(R^{2}, " statistics")),
       color = "")
# => the correlations are high, and indicate that, across models, 
#    there are large within-resample correlations. 
# => within-resample effect, where results for the same resample 
#    tend to be similar

rsq_wider %>% 
  with(cor.test(basic_lm, splines_lm)) %>% 
  tidy() %>% 
  select(estimate, starts_with("conf"))
# => within-resample correlation appears to be real...
# => ignoring the resample-to-resample effect would bias our model 
#    comparisons towards finding no differences between models.


## simple hypothesis testing methods --------------------------------------
# A simple and fast method for comparing two models at a time is to use 
# the differences in R2 values as the outcome data in the ANOVA model.
compare_lm <- rsq_wider %>% 
  mutate(difference = splines_lm - basic_lm)

lm(difference ~ 1, 
   data = compare_lm) %>% 
  tidy(conf.int = TRUE) %>% 
  select(estimate, p.value, starts_with("conf"))
# OR
rsq_wider %>% 
  with(t.test(splines_lm, basic_lm,
              paired = TRUE)) %>% 
  tidy() %>% 
  select(estimate, p.value, starts_with("conf"))

### p-values: 
# “Informally, a p-value is the probability under a specified statistical 
# model that a statistical summary of the data (e.g., the sample mean difference 
# between two compared groups) would be equal to or more extreme than its observed value.”
# - Wassertein & Lazar 2016

# => the collection of spline terms for longitude and latitude do appear to have an effect. 


## Bayesian methods -------------------------------------------------------
### a random intercept model
# we assume that the resamples impact the model only by changing the intercept. 
# NOTE: This constrains the resamples from having a differential impact on the 
# regression parameters βj; these are assumed to have the same relationship across 
# resamples.

library(tidyposterior)
library(rstanarm)

rsq_anova <- perf_mod(four_models,
                      metric = "rsq",
                      prior_intercept = rstanarm::student_t(df = 1),
                      chains =  4,
                      iter = 5000,
                      seed = 1102)
rsq_anova

model_post <- rsq_anova %>% 
  # Take a random sample from the posterior distribution
  # so set the seed again to be reproducible. 
  tidy(seed = 1103)
glimpse(model_post)

model_post %>% 
  mutate(model = fct_relevel(model,
                             "basic_lm", "interact_lm", 
                             "splines_lm", "random_forest")) %>% 
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50,
                 color = "white",
                 fill = "blue",
                 alpha = 0.5) +
  facet_wrap(~ model,
             ncol = 1) +
  labs(title = "Posterior distribution for the coefficient of determiniation")
# These histograms describe the estimated probability distributions of the mean R2 
# value for each model. There is some overlap, especially for the three linear models.

# OR
rsq_anova %>% 
  autoplot() + 
  geom_text(aes(label = workflow),
            vjust = -1,
            hjust = -0.1) +
  expand_limits(x = c(1, 4.5)) +
  theme(legend.position = "none")


### compare two linear models
rsq_diff <- contrast_models(rsq_anova,
                            list_1 = "splines_lm",
                            list_2 = "basic_lm",
                            seed = 1104)
rsq_diff

rsq_diff %>% 
  ggplot(aes(x = difference)) + 
  geom_histogram(bins = 50,
                 color = "white",
                 fill = "red",
                 alpha = 0.5) + 
  geom_vline(xintercept = 0,
             lty = 2) +
  labs(title = expression(paste("Posterior for mean difference in ", R^{2})))
# The posterior shows that the center of the distribution is greater than zero 
# (indicating that the model with splines typically had larger values) but does 
# overlap with zero to a degree. 

rsq_diff %>% 
  summary() %>% 
  select(-starts_with("pract"))
# The probability column reflects the proportion of the posterior that is greater 
# than zero. This is the probability that the positive difference is real. The value 
# is not close to zero, providing a strong case for statistical significance, i.e., 
# the idea that statistically the actual difference is not zero.

# With a posterior distribution, we can also compute the probability of being practically 
# significant (ROPE estimate (for Region Of Practical Equivalence | we set a threshold of 2%)
rsq_diff %>% 
  summary(size = 0.02) %>% 
  select(starts_with("pract"))
# This large value indicates that, for our effect size, there is an overwhelming probability 
# that the two models are practically the same. Even though the previous plot showed that our 
# difference is likely nonzero, the equivalence test suggests that it is small enough to not 
# be practically meaningful.


## compare all models at once ---------------------------------------------
rsq_anova %>% 
  autoplot(type = "ROPE",
           size = 0.02) +
  geom_text(aes(label = workflow),
            vjust = -0.5,
            hjust = -0.1) +
  expand_limits(x = c(1, 4.25)) +
  theme(legend.position = "none")
# none of the linear models comes close to the random forest model when a 2% 
# practical effect size is used.

# NOTE: More resamples increases the precision of the overall resampling estimate; 
#       that precision propagates to this type of analysis. However at some point
#       the there are diminishing returns... The "right" number of resamples depends
#       on the data set as well.





# CLEAN UP FIRST ----------------------------------------------------------
rm(list = ls())
.rs.restartR()






# Chapter 12 - Model Tuning and the Dangers of Overfitting ----------------

library(tidyverse)
library(tidymodels)
theme_set(theme_minimal())

ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  janitor::clean_names()


### training and testing set
set.seed(502)
ames_split <- initial_split(ames, 
                            prop = 0.8,
                            strata = sale_price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

# Overfitting is the situation where a model adapts too much to the training 
# data; it performs well for the data used to build the model but poorly for 
# new data. Since tuning model parameters can increase model complexity, poor 
# choices can lead to overfitting.
# The solution for detecting when a model is overemphasizing the training 
# set is using out-of-sample data.

### define neural network (with tuning parameter)
neural_net_spec <- mlp(hidden_units = tune()) %>% 
  set_engine("keras") %>% 
  set_mode("regression")

extract_parameter_set_dials(neural_net_spec)


### create recipe (with tuning parameters)
ames_rec <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built +
                     bldg_type + longitude + latitude,
                   data = ames_train) %>% 
  step_log(gr_liv_area,
           base = 10) %>% 
  step_other(neighborhood,
             threshold = tune()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ gr_liv_area:starts_with("bldg_type_")) %>% 
  step_ns(longitude, deg_free = tune("longitude df")) %>% 
  step_ns(latitude, deg_free = tune("latitude df"))


### get tunable model parameters
recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param

wflow_param <- workflow() %>% 
  add_model(neural_net_spec) %>% 
  add_recipe(ames_rec) %>% 
  extract_parameter_set_dials()
wflow_param
# Neural networks are exquisitely capable of emulating nonlinear patterns. 
# Adding spline terms to this type of model is unnecessary; we combined this 
# model and recipe for illustration only.

# update parameter in place
# 1)
ames_rec %>% 
  extract_parameter_set_dials() %>% 
  update(threshold = threshold(c(0.8, 1.0))) %>% 
  extract_parameter_dials("threshold")

# 2)
rf_spec <- rand_forest(mtry = tune()) %>% 
  set_engine("ranger",
             regularization.factor = tune("regularization")) %>% 
  set_mode("regression")

rf_param <- rf_spec %>% 
  extract_parameter_set_dials()

rf_param %>% 
  update(mtry = mtry(c(1, 70)))

# 3)
pca_rec <- recipe(sale_price ~ .,
                  data = ames_train) %>% 
  # Select the square-footage predictors and extract their PCA components:
  step_normalize(contains("sf")) %>% 
  # Select the number of components needed to capture 95% of
  # the variance in the predictors. 
  step_pca(contains("sf"),
           threshold = 0.95)

updated_param <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(pca_rec) %>% 
  extract_parameter_set_dials() %>% 
  finalize(ames_train)

updated_param %>% 
  extract_parameter_dials("mtry")

# 4) 
# some parameters are on a transformed scale
penalty()

# updating needs to be on the same scale
# correct
penalty(c(-1, 0)) %>% 
  value_sample(1000) %>% 
  summary()

# incorrect
penalty(c(0.1, 1)) %>% 
  value_sample(1000) %>% 
  summary()






# Chapter 13 - Grid Search ------------------------------------------------

mlp_spec <- mlp(hidden_units = tune(),
                penalty = tune(),
                epochs = tune()) %>% 
  set_engine(engine = "nnet",
             # prevents extra logging of
             # training process
             trace = 0) %>% 
  set_mode("classification")

mlp_param <- mlp_spec %>% 
  extract_parameter_set_dials()
mlp_param

mlp_param %>% 
  extract_parameter_dials("hidden_units")
mlp_param %>% 
  extract_parameter_dials("penalty")
mlp_param %>% 
  extract_parameter_dials("epochs")


### regular grids
crossing(
  hidden_units = 1:3,
  penalty = c(0.0, 0.1),
  epochs = c(100, 200)
)

grid_regular(mlp_param,
             levels = 2)

grid_regular(mlp_param,
             levels = c(hidden_units = 3,
                        penalty = 2,
                        epochs = 2))

# One advantage to using a regular grid is that the relationships and patterns 
# between the tuning parameters and the model metrics are easily understood. 
# The factorial nature of these designs allows for examination of each parameter 
# separately with little confounding between parameters.


### irregular grids
set.seed(1301)
mlp_param %>% 
  grid_random(size = 1000)
# NOTE: For penalty, the random numbers are uniform on the log (base-10) scale 
#       but the values in the grid are in the natural units.

# The issue with random grids is that, with small-to-medium grids, random values 
# can result in overlapping parameter combinations. Also, the random grid needs to 
# cover the whole parameter space, but the likelihood of good coverage increases 
# with the number of grid values.

library(ggforce)
set.seed(1302)

mlp_param %>% 
  grid_random(size = 20, 
              # keeps penalty in log10 units
              original = FALSE) %>% 
  ggplot(aes(x = .panel_x,
             y = .panel_y)) +
  geom_point() + 
  facet_matrix(vars(hidden_units,
                    penalty,
                    epochs),
               layer.diag = 2) + 
  theme_light()

# using space-filling designs can help (here: Latin hypercube):
set.seed(1303)

mlp_param %>% 
  grid_latin_hypercube(size = 20,
                       original = FALSE) %>% 
  ggplot(aes(x = .panel_x,
             y = .panel_y)) +
  geom_point() + 
  facet_matrix(vars(hidden_units,
                    penalty,
                    epochs),
               layer.diag = 2) + 
  theme_light()

# While not perfect, this Latin hypercube design spaces the points farther 
# away from one another and allows a better exploration of the hyperparameter 
# space. Space-filling designs can be very effective at representing the parameter 
# space. The default design used by the tune package is the maximum entropy design. 
# These tend to produce grids that cover the candidate space well and drastically 
# increase the chances of finding good results.


### evaluating the grid
# To choose the best tuning parameter combination, each candidate set is assessed 
# using data that were not used to train that model.
# => resampling methods or a single validation set

data(cells)
cells <- cells %>% 
  select(-case)

set.seed(1304)
cell_folds <- vfold_cv(data = cells, 
                       v = 10)
cell_folds

### create recipe
mlp_rec <- recipe(class ~ .,
                  data = cells) %>% 
  # transformation step (more symmetric distriburion)
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), 
           num_comp = tune()) %>% 
  # While the resulting PCA components are technically 
  # on the same scale, the lower-rank components tend 
  # to have a wider range than the higher-rank components. 
  # => normalize again to coerce the predictors to have 
  # the same mean and variance.
  step_normalize(all_numeric_predictors())

# NOTE: In step_pca(), using zero PCA components is 
#       a shortcut to skip the feature extraction. 
#       In this way, the original predictors can be 
#       directly compared to the results that include 
#       PCA components.


### create workflow
mlp_wflow <- workflow() %>% 
  add_model(mlp_spec) %>% 
  add_recipe(mlp_rec)


### update parameters
# before
mlp_wflow %>% 
  extract_parameter_set_dials() %>% 
  extract_parameter_dials("epochs")

mlp_wflow %>% 
  extract_parameter_set_dials() %>% 
  extract_parameter_dials("num_comp")

# update
mlp_param <- mlp_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    epochs = epochs(c(50, 200)),
    num_comp = num_comp(c(0, 40))
  )

# after
mlp_param %>% 
  extract_parameter_dials("epochs")

mlp_param %>% 
  extract_parameter_dials("num_comp")



### create grid and evaluate
roc_res <- metric_set(roc_auc)
set.seed(1305)

# parallel processing with doSNOW
library(doSNOW)
ctemp <- makeCluster(16L)
registerDoSNOW(ctemp)

mlp_reg_tune <- mlp_wflow %>% 
  tune_grid(
    cell_folds,
    grid = mlp_param %>% grid_regular(levels = 3),
    metrics = roc_res,
    control = control_grid(
      save_pred = TRUE
      # does not work when using parallel processing...
      # verbose = TRUE
    ))

# stop the doSNOW-cluster
stopCluster(cl = ctemp)

autoplot(mlp_reg_tune)
mlp_reg_tune %>% 
  show_best()
# NOTE: tune_grid() does not fit a final model. We have to select
#       the model parameters first.

# select_best()
logistic_param <- mlp_reg_tune %>% 
  select_best(metric = "roc_auc")

# manually
logistic_param <- tibble(
  num_comp = 0,
  epochs = 200,
  hidden_units = 5,
  penalty = 1
)


final_mlp_wflow <- mlp_wflow %>% 
  finalize_workflow(logistic_param)
final_mlp_wflow

### fit
final_mlp_fit <- final_mlp_wflow %>% 
  fit(cells)
# NOTE: If you did not use a workflow, finalization of a model and/or 
#       recipe is done using finalize_model() and finalize_recipe().


## racing methods ---------------------------------------------------------

# Using racing methods can speed-up the search. Only good enough models
# or combinations/configurations of different tuning-parameters are kept.
# => interim analysis techniques

library(finetune)

set.seed(1308)
mlp_sfd_race <-
  mlp_wflow %>%
  tune_race_anova(
    cell_folds,
    grid = 20,
    param_info = mlp_param,
    metrics = roc_res,
    control = control_race(verbose_elim = TRUE)
  )

show_best(mlp_sfd_race, n = 10)





# CLEAN UP FIRST ----------------------------------------------------------
rm(list = ls())
.rs.restartR()





# Chapter 14 - Iterative Search -------------------------------------------

library(tidymodels)

data(cells)
cells <- cells %>% select(-case)

set.seed(1304)
cell_folds <- vfold_cv(cells)

roc_res <- metric_set(roc_auc)

# When grid search is infeasible or inefficient, iterative methods are a 
# sensible approach for optimizing tuning parameters.


### SVM
svm_rec <- recipe(class ~ .,
                  data = cells) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

svm_spec <- svm_rbf(cost = tune(),
                    rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

svm_wflow <- workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(svm_rec)


# tuning parameter
cost()
rbf_sigma()

svm_param <- svm_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(rbf_sigma = rbf_sigma(c(-7, -1)))

svm_param %>% 
  extract_parameter_dials("rbf_sigma")


### 1. search procedure
set.seed(1401)
start_grid <- svm_param %>% 
  update(cost = cost(c(-6, 1)),
         rbf_sigma = rbf_sigma(c(-6, -4))) %>% 
  grid_regular(levels = 2)
start_grid

set.seed(1402)
svm_initial <- svm_wflow %>% 
  tune_grid(resamples = cell_folds,
            grid = start_grid,
            metrics = roc_res,
            control = control_grid(verbose = TRUE))
svm_initial
collect_metrics(svm_initial)
# This initial grid shows fairly equivalent results, with no 
# individual point much better than any of the others. These 
# results can be ingested by the iterative tuning functions 
# discussed in the following sections to be used as initial values.


### 2. Baysian optimization


