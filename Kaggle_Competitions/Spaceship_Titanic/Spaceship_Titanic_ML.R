
####################################### Spaceship Titanic ####################################### 


# load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(tidylog)
library(vip)
library(skimr)
library(naniar)
library(corrplot)
library(glue)

theme_set(theme_light())


# load data ---------------------------------------------------------------

train_titanic <- read_csv("Kaggle_Competitions/Spaceship_Titanic/train.csv")
test_titanic <- read_csv("Kaggle_Competitions/Spaceship_Titanic/test.csv")



# data dictionary ---------------------------------------------------------

# PassengerId               A unique Id for each passenger. Each Id takes the form gggg_pp where 
#                           gggg indicates a group the passenger is travelling with and pp is their 
#                           number within the group. People in a group are often family members, but 
#                           not always.

# HomePlanet                The planet the passenger departed from, typically their planet of permanent 
#                           residence.

# CryoSleep                 Indicates whether the passenger elected to be put into suspended animation 
#                           for the duration of the voyage. Passengers in cryosleep are confined to 
#                           their cabins.

# Cabin                     The cabin number where the passenger is staying. Takes the form deck/num/side, 
#                           where side can be either P for Port or S for Starboard.

# Destination               The planet the passenger will be debarking to.

# Age                       The age of the passenger.

# VIP                       Whether the passenger has paid for special VIP service during the voyage.

# RoomService,            
# FoodCourt,            
# ShoppingMall,             Amount the passenger has billed at each of the Spaceship Titanic's many luxury amenities. 
# Spa,            
# VRDeck            

# Name                      The first and last names of the passenger.

# Transported               Whether the passenger was transported to another dimension. This is the target, 
#                           the column you are trying to predict.



# load names data
female_names <- readtext::readtext("Kaggle_Competitions/Spaceship_Titanic/female.txt") %>% 
  str_split("\n") %>% 
  unlist()

male_names <- readtext::readtext("Kaggle_Competitions/Spaceship_Titanic/male.txt") %>% 
  str_split("\n") %>% 
  unlist()


# okay this does not get us further...
train_titanic %>% 
  separate(Name, into = c("First_name", "Last_name"), sep = " ") %>% 
  mutate(Gender = case_when(First_name %in% female_names ~ "Female",
                            First_name %in% male_names ~ "Male",
                            TRUE ~  "NA")) %>%
  count(Last_name, sort = TRUE)


# lets take a look at the cabin number (split them up first)
train_titanic <- train_titanic %>% 
  separate(Cabin, into = c("Cabin_Deck", "Cabin_Number", "Cabin_Side"),
           remove = FALSE)

test_titanic <- test_titanic %>% 
  separate(Cabin, into = c("Cabin_Deck", "Cabin_Number", "Cabin_Side"),
           remove = FALSE)


# check for people who stayed together (families, couples etc.)
Cabin_together_train <- train_titanic %>% 
  select(PassengerId, Cabin) %>% 
  group_by(Cabin) %>% 
  mutate(N_people_cabin = n()) %>% 
  arrange(desc(N_people_cabin)) %>% 
  ungroup() 


Cabin_together_test <- test_titanic %>% 
  select(PassengerId, Cabin) %>% 
  group_by(Cabin) %>% 
  mutate(N_people_cabin = n()) %>% 
  arrange(desc(N_people_cabin)) %>% 
  ungroup() 



train_titanic <- train_titanic %>% 
  left_join(Cabin_together_train, by = c("PassengerId", "Cabin")) %>% 
  mutate(N_people_cabin,
         N_people_cabin = ifelse(N_people_cabin > 20, NA, N_people_cabin),
         Together_cabin = ifelse(N_people_cabin > 1, TRUE, FALSE))

test_titanic <- test_titanic %>% 
  left_join(Cabin_together_test, by = c("PassengerId", "Cabin")) %>% 
  mutate(N_people_cabin,
         N_people_cabin = ifelse(N_people_cabin > 20, NA, N_people_cabin),
         Together_cabin = ifelse(N_people_cabin > 1, TRUE, FALSE))




# some EDA ----------------------------------------------------------------

glimpse(train_titanic)

## missing values ---------------------------------------------------------

# NAs absolut
map_df(train_titanic, ~sum(is.na(.)))
# NAs percent
map_df(train_titanic, ~mean(is.na(.)) * 100)

# upset plot
train_titanic %>% 
  gg_miss_upset(nsets = n_var_miss(train_titanic), 
                nintersects = NA)

# skim
skim(train_titanic)
# does not look too good NA-wise (if we drop all NAs we will lose about 25% of our data) 
# => we will have to impute some values!



## some counting ----------------------------------------------------------

for(var in names(train_titanic)) {
  
  train_titanic %>% 
    count(.data[[var]],
          sort = TRUE) %>% 
    print()
}



## plotting ---------------------------------------------------------------


# create data set for plotting (only keep numerical variables and remove identifiers)

plot_data <- train_titanic %>% 
  drop_na() %>% 
  select(-c(PassengerId, Name, Cabin_Number, Cabin)) %>% 
  mutate(Transported = as.factor(Transported))

plot_data_long <- train_titanic %>%
  drop_na() %>% 
  select(-c(PassengerId, Name, Cabin, Cabin_Deck, Cabin_Number, Cabin_Side, HomePlanet, Destination, CryoSleep, VIP, Together_cabin)) %>% 
  mutate(Transported = as.factor(Transported)) %>% 
  pivot_longer(cols = -Transported, names_to = "key", values_to  = "value")



### 1. distribution plots (numeric variables)

# feature distribution plots 
plot_data_long %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~ key, scales = "free")
# need to transform FoodCourt, RoomService, ShoppingMall, Spa, VRDeck
# => log10


# feature distribution plots by Transported
plot_data_long %>% 
  filter(key == "Age") %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(fill = as.factor(Transported)))

plot_data_long %>% 
  filter(key != "Age") %>% 
  ggplot(aes(x = value)) + 
  geom_density(aes(fill = as.factor(Transported)),
               alpha = 0.5) + 
  facet_wrap(~ key, scales = "free") + 
  scale_x_log10()
# NOTE: there are a lot of 0s. Do not forget to set an offset = 1



### 2. feature boxplots (numeric variables)

# feature boxplots by Transported
plot_data_long %>% 
  filter(key == "Age") %>% 
  ggplot(aes(x = value)) + 
  geom_boxplot(aes(fill = as.factor(Transported)),
               outlier.alpha = 0.5) + 
  facet_wrap(~ key, scales = "free")

plot_data_long %>% 
  filter(key != "Age") %>% 
  ggplot(aes(x =value))  +
  geom_boxplot(aes(fill = as.factor(Transported)),
               alpha = 0.5) + 
  facet_wrap(~ key, scales = "free") +
  scale_x_log10()



### 3. bar plots (categorical variables)

gridExtra::grid.arrange(
  
  
  # 3.1 Cabin_Deck
  plot_data %>%
    group_by(Transported, Cabin_Deck) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(Cabin_Deck) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = Cabin_Deck, y = freq)) +
    geom_bar(aes(fill = Transported), 
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  
  # 3.2 Cabin_Side
  plot_data %>%
    group_by(Transported, Cabin_Side) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(Cabin_Side) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = Cabin_Side, y = freq)) +
    geom_bar(aes(fill = Transported), 
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  
  
  # 3.3 HomePlanet
  plot_data %>%
    group_by(Transported, HomePlanet) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(HomePlanet) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = HomePlanet, y = freq)) +
    geom_bar(aes(fill = Transported), 
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  # 3.4 CryoSleep
  plot_data %>%
    group_by(Transported, CryoSleep) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(CryoSleep) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = CryoSleep, y = freq)) +
    geom_bar(aes(fill = Transported), 
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  # 3.5 Destination
  plot_data %>%
    group_by(Transported, Destination) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(Destination) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = Destination, y = freq)) +
    geom_bar(aes(fill = Transported), 
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  
  
  # 3.6 VIP
  plot_data %>%
    group_by(Transported, VIP) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(VIP) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = VIP, y = freq)) +
    geom_bar(aes(fill = Transported), 
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  # 3.7 Together_cabin
  plot_data %>% 
    group_by(Transported, Together_cabin) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(Together_cabin) %>% 
    mutate(total = sum(n),
           freq = n/total) %>% 
    ungroup() %>% 
    ggplot(aes(x = Together_cabin, y = freq))  +
    geom_bar(aes(fill = Transported),
             stat = "identity", 
             position = "dodge") +
    geom_text(aes(label = scales::percent(freq, accuracy = 0.1), 
                  group = Transported), 
              stat = "identity", 
              position = position_dodge(width = 1), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Percent"),
  
  
  
  ncol = 2
  
)



### 3. correlation plot

train_titanic %>% 
  select(-c(PassengerId, Cabin_Number, Cabin, Name)) %>% 
  drop_na() %>% 
  mutate(HomePlanet = as.factor(HomePlanet),
         HomePlanet = as.numeric(HomePlanet),
         Cabin_Deck = as.factor(Cabin_Deck),
         Cabin_Deck = as.numeric(Cabin_Deck),
         Cabin_Side = as.factor(Cabin_Side),
         Cabin_Side = as.numeric(Cabin_Side),
         Destination = as.factor(Destination),
         Destination = as.numeric(Destination)) %>% 
  cor() %>% 
  corrplot.mixed(order = "hclust",
                 upper = "circle",
                 lower = "number",
                 tl.pos = 'lt',
                 tl.col = "black",
                 lower.col = "black",
                 number.cex = 1)



## results of the EDA -----------------------------------------------------

### good predictors

# numerical
# - RoomService => log
# - Spa => log
# - VRDeck => log
# - Age
# - FoodCourt => log
# - (ShoppingMall) => log

# => do not forget the offset = 1

# categorical
# - CryoSleep
# - Together_cabin
# - VIP
# - HomePlanet and Destination 

# Need to impute missing values!



# Modeling ----------------------------------------------------------------

# a stupid model would achieve this score
sd(train_titanic$Transported)
# how much better can we do?


## prepare data -----------------------------------------------------------

train <- train_titanic %>% 
  # remove identifiers
  select(-c(PassengerId, Name, Cabin_Number, Cabin, N_people_cabin)) %>% 
  # make sure the target variable and logicals are a classes (binary)
  mutate(Transported = as.factor(Transported),
         CryoSleep = as.factor(CryoSleep),
         VIP = as.factor(VIP),
         Together_cabin = as.factor(Together_cabin))


## create train and test --------------------------------------------------

# we create a train and test data set out of the train_titanic
# => is needed for a better model evaluation
set.seed(1234) 
train_split <- train %>% 
  initial_split(prop = 0.80, strata = Transported)

train_ML <- training(train_split)

test_ML <- testing(train_split)


## create folds -----------------------------------------------------------
set.seed(1234)
train_ML_folds <- vfold_cv(data = train_ML, 
                           v = 5,
                           strata = Transported)


## Parallelisation --------------------------------------------------------

cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)
showConnections()



## data pre-processing ----------------------------------------------------

# Define recipe specification
df_recipe <- recipe(Transported ~ ., data = train_ML) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_log(RoomService, Spa, VRDeck, FoodCourt, ShoppingMall, 
           offset = 1, 
           base = 10) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(CryoSleep, VIP, HomePlanet, Destination,
             Cabin_Deck, Cabin_Side, Together_cabin,
             one_hot = TRUE)


# let's take a look at the data
train_ML_baked <- df_recipe %>% 
  prep(training = train_ML) %>% 
  bake(new_data = train_ML)

View(train_ML_baked)
skim(train_ML_baked)


## Specify model type and computational engine ----------------------------

# glmnet
glmnet_model <- logistic_reg() %>% 
  set_engine("glmnet") %>% 
  set_mode("classification") %>%
  set_args(penalty = tune(),
           mixture = tune())



# RandomForest
rf_model <- rand_forest() %>% 
  set_engine("randomForest") %>%
  set_mode("classification") %>% 
  set_args(mtry = tune(), 
           trees = tune(),
           min_n = tune())

# Ranger
ranger_model <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(), 
           trees = tune(),
           min_n = tune())


# XGBoost
xgboost_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification") %>%
  set_args(mtry = tune(), 
           trees = tune(), 
           learn_rate = tune(), 
           tree_depth = tune(), 
           min_n = tune())





## Create a regular tune grid ---------------------------------------------

# use grid_random instead of grid_regular (otherwise it takes too long...)

glmnet_grid <- grid_regular(range_set(penalty(), c(0.1, 1)),
                            range_set(mixture(), c(0.1, 1)),
                            levels = 4) %>% 
  mutate(penalty = penalty / 10)


rr_grid <- grid_random(range_set(mtry(), c(4, 8)),
                       range_set(trees(), c(500, 1000)),
                       range_set(min_n(), c(1, 5)),
                       size = 20)

xgboost_grid <- grid_random(range_set(mtry(), c(4, 8)),
                            range_set(trees(), c(500, 1000)),
                            range_set(learn_rate(trans = NULL), c(0.01, 0.02)),
                            range_set(tree_depth(), c(6, 12)),
                            range_set(min_n(), c(1, 5)),
                            size = 20)







## Create model workflow --------------------------------------------------

glmnet_wflow <- workflow() %>% 
  add_model(glmnet_model) %>% 
  add_recipe(df_recipe)

rf_wflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(df_recipe)


ranger_wflow <- workflow() %>% 
  add_model(ranger_model) %>% 
  add_recipe(df_recipe)

xgboost_wflow <- workflow() %>% 
  add_model(xgboost_model) %>% 
  add_recipe(df_recipe)



## Analyse resamples with hyperparameter tuning ---------------------------

# glmnet
system.time({
  set.seed(1234)
  glmnet_results <- glmnet_wflow %>% 
    tune_grid(resamples = train_ML_folds, 
              grid = glmnet_grid, 
              control = control_grid(verbose = TRUE,
                                     save_pred = TRUE),
              metrics = metric_set(accuracy, roc_auc, sens, spec))
})



# RandomForest
system.time({
  set.seed(1234)
  rf_results <- rf_wflow %>% 
    tune_grid(resamples = train_ML_folds, 
              grid = rr_grid, 
              control = control_grid(verbose = TRUE,
                                     save_pred = TRUE),
              metrics = metric_set(accuracy, roc_auc, sens, spec))
})



# ranger
system.time({
  set.seed(1234)
  ranger_results <- ranger_wflow %>% 
    tune_grid(resamples = train_ML_folds, 
              grid = rr_grid, 
              control = control_grid(verbose = TRUE,
                                     save_pred = TRUE),
              metrics = metric_set(accuracy, roc_auc, sens, spec))
})



# XGBoost
system.time({
  set.seed(1234)
  xgboost_results <- xgboost_wflow %>% 
    tune_grid(resamples = train_ML_folds, 
              grid = xgboost_grid, 
              control = control_grid(verbose = TRUE,
                                     save_pred = TRUE),
              metrics = metric_set(accuracy, roc_auc, sens, spec))
})





### evaluate hyperparameter

# autoplot is not very useful here...
autoplot(glmnet_results)
autoplot(rf_results)
autoplot(ranger_results)
autoplot(xgboost_results)


show_best(x = glmnet_results,
          metric = "accuracy")
show_best(x = rf_results,
          metric = "accuracy")
show_best(x = ranger_results,
          metric = "accuracy")
show_best(x = xgboost_results,
          metric = "accuracy")



xgboost_results %>% 
  collect_metrics() %>% 
  filter(.metric == "accuracy") %>% 
  select(-c(.estimator, .config)) %>% 
  pivot_longer(cols = -c(.metric, mean, n, std_err), names_to = "parameter", values_to = "value") %>% 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(show.legend = FALSE) +
  labs(x = "Value",
       y = "Mean Accuracy") +
  facet_wrap(~ parameter, scales = "free")



## Finalise model workflow ------------------------------------------------

best_xgboost <- select_best(x = xgboost_results, 
                            metric = "accuracy")

final_wf <- 
  xgboost_wflow %>% 
  finalize_workflow(best_xgboost)

final_wf %>% 
  fit(data = train_ML) %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = ncol(train_ML_baked) - 1)


final_xgboost <- fit(final_wf, train_ML)



## evaluate on the test_ML ------------------------------------------------

# predict it on the unseen data first and evaluate

test_ML

xgboost_pred_prob <- predict(final_xgboost, 
                             new_data = test_ML, 
                             type = "prob")
xgboost_pred_class <- predict(final_xgboost, 
                              new_data = test_ML, 
                              type = "class")


predictions <- test_ML %>% 
  bind_cols(., xgboost_pred_prob, xgboost_pred_class)


#### evaluate

# metrics
multi_metric <- metric_set(accuracy, sens, spec)
multi_metric(predictions, truth = Transported, estimate = .pred_class)

# confusion matrix
conf_mat(predictions, truth = Transported, estimate = .pred_class)


# roc curve
predictions %>% 
  mutate(.pred_class = as.numeric(.pred_class) - 1) %>% 
  roc_curve(Transported, .pred_class, event_level = "second") %>% 
  autoplot()






###############################################################################################
###############################################################################################
############################                                       ############################ 
############################ ONLY NOW we use the real testing data ############################ 
############################                                       ############################ 
###############################################################################################
###############################################################################################

# predict the real test data ----------------------------------------------

test_ML_final <- test_titanic %>%   
  # make sure the target variable is a class (binary)
  mutate(CryoSleep = as.factor(CryoSleep),
         VIP = as.factor(VIP))


xgboost_pred_prob_final <- predict(final_xgboost, 
                                   new_data = test_ML_final, 
                                   type = "prob")

xgboost_pred_class_final <- predict(final_xgboost, 
                                    new_data = test_ML_final, 
                                    type = "class")

# Data frame from test set with the model predictions “attached”
predictions_final <-  test_ML_final %>% 
  bind_cols(., xgboost_pred_prob_final, xgboost_pred_class_final)


predictions_final %>% 
  select(PassengerId, .pred_class) %>% 
  rename("Transported" = .pred_class) %>% 
  # Needs to be exactly as in the description!
  mutate(Transported = str_to_title(Transported)) %>% 
  write_csv(file = "Kaggle_Competitions//sample_submission.csv")


# Stop parallelisation ----------------------------------------------------
parallel::stopCluster(cl)
closeAllConnections()
