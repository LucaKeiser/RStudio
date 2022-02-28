# A brief Overview of Functional Programming with purrr

# load packages
library(tidyverse)
library(tidymodels)
library(broom)


# mutate vs. purrr --------------------------------------------------------

iris %>% 
  mutate(sqrt_sepal_length = sqrt(Sepal.Length),
         # if you just use map it returns a list... 
         # use map_dbl directly!
         map_sepal_length = map_dbl(Sepal.Length, sqrt))




# use map for many models -------------------------------------------------

## first example
iris %>% 
  
  # nest data by Species => 3 tibbles (for every species one...)
  nest(data = -Species) %>% 
  
  # use map to create a model for every species!
  mutate(model = map(data, ~lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = .x)),
  
         # use map and broom to tidy the models
         tidy_model = map(model, broom::tidy)) %>% 
  
  select(Species, tidy_model) %>% 
  unnest(cols = c(tidy_model)) %>% 
  ggplot(aes(x = term, y = estimate, color = Species)) +
  geom_point()



## second example
set.seed(54)

# create model data
model_data <- iris %>% 
  nest(data = -Species) %>% 
  
  # initial split => create training and testing data (cross validation)
  mutate(data_splits = map(data, rsample::initial_split, prop  = 0.8),
         train_data = map(data_splits, rsample::training),
         test_data = map(data_splits, rsample::testing))


# random forest-model
rf_model <- parsnip::rand_forest(trees = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("randomForest")


# create k folds and recipe
model_data <- model_data %>% 
  
  # you can also include self-written functions!
  mutate(k_folds_data = map(train_data, vfold_cv),
         
         recipe = map(train_data, .f = function(data){
    # use all variables to predict Sepal.Length
    recipe(Sepal.Length ~ ., data = data)
  }))

# create a grid with the model's parameters
rf_grid <- grid_regular(parameters(rf_model))



# write train_models-function
train_models <- function(recipe, k_fold_data){
  tune_grid(
    rf_model,
    # training set is in the recipe
    recipe,
    grid = rf_grid,
    resamples = k_fold_data
  )
}


# train the model!
model_data <- model_data %>% 
  mutate(tune_results = map2(recipe, k_folds_data, train_models))


# take a look at the parameters
model_data <- model_data %>% 
  # rmse = root mean square error
  mutate(parameters = map(tune_results, ~show_best(.x, "rmse", n = 1)),
         
         # finalize the model!
         final_model = map2(parameters, recipe, .f = function(x, y){
           workflow() %>% 
             add_model(rf_model) %>% 
             add_recipe(y) %>% 
             finalize_workflow(x)
         }),
         eval = map2(final_model, data_splits, last_fit))


model_data %>% 
  mutate(metrics = map(eval, collect_metrics)) %>% 
  select(Species, metrics) %>% 
  unnest(cols = c(metrics))




# plotting ----------------------------------------------------------------
iris %>% 
  nest(data = -Species) %>% 
  mutate(chart = map(data, .f = function(data){
    ggplot(data = data, aes(x = Sepal.Length, y = Sepal.Width)) + 
      geom_point() 
  })) %>% 
  pull(chart)
