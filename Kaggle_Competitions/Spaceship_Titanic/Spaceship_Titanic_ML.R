
####################################### Spaceship Titanic ####################################### 


# load packages -----------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(tidylog)
library(skimr)
library(naniar)
library(corrplot)
library(glue)

theme_set(theme_light())


# load data ---------------------------------------------------------------

train <- read_csv("Kaggle_Competitions/Spaceship_Titanic/train.csv")
test <- read_csv("Kaggle_Competitions/Spaceship_Titanic/test.csv")



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




# some EDA ----------------------------------------------------------------

## missing values ---------------------------------------------------------

# NAs absolut
map_df(train, ~sum(is.na(.)))
# NAs percent
map_df(train, ~mean(is.na(.)) * 100)

# upset plot
train %>% 
  gg_miss_upset(nsets = n_var_miss(train), nintersects = NA)

# skim
skim(train)
# does not look too good NA-wise (if we drop all NAs we will lose about 25% of our data) 
# => we will have to impute some values!



## some counting ----------------------------------------------------------

for(var in names(train)) {
  
  train %>% 
    count(.data[[var]],
          sort = TRUE) %>% 
    print()
}



## plotting ---------------------------------------------------------------


# create data set for plotting (only keep numerical variables and remove identifiers)

plot_data <- train %>% 
  drop_na() %>% 
  select(-c(PassengerId, Name, Cabin)) %>% 
  mutate(Transported = as.factor(Transported))

plot_data_long <- train %>%
  drop_na() %>% 
  select(-c(PassengerId, Cabin, Name, HomePlanet, Destination, CryoSleep, VIP)) %>% 
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
  
  
  # 3.1 HomePlanet
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
  
  
  # 3.2 CryoSleep
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
  
  
  # 3.3 Destination
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
  
  
  
  
  # 3.4 VIP
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
  
  
  ncol = 2
  
)



### 3. correlation plot

train %>% 
  select(-c(PassengerId, HomePlanet, Cabin, Destination, Name)) %>%
  drop_na() %>% 
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
# - VIP
# - HomePlanet and Destination (but why?)

# Need to impute missing values!



# Modeling ----------------------------------------------------------------

# a stupid model would achieve this score
sd(train$Transported)
# how much better can we do?


## Parallelisation --------------------------------------------------------

cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(4)
doParallel::registerDoParallel(cl)
showConnections()



## prepare data -----------------------------------------------------------

train_ML <- train %>% 
  # remove identifiers
  select(-c(PassengerId, Cabin, Name)) %>% 
  # make sure the target variable is a class (binary)
  mutate(Transported = as.factor(Transported),
         CryoSleep = as.factor(CryoSleep),
         VIP = as.factor(VIP))



## create folds -----------------------------------------------------------
set.seed(1234)
train_ML_folds <- vfold_cv(data = train_ML, 
                           v = 10,
                           strata = Transported)





## data pre-processing ----------------------------------------------------

# Define recipe specification
df_recipe <- recipe(Transported ~ ., data = train_ML) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_log(RoomService, Spa, VRDeck, FoodCourt, ShoppingMall, 
           offset = 1, 
           base = 10) %>% 
  step_dummy(CryoSleep, VIP, HomePlanet, Destination, 
             one_hot = TRUE)



## Specify model type and computational engine ----------------------------

xgboost_model <- 
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification") %>%
  set_args(mtry = tune(), 
           trees = tune(), 
           learn_rate = tune(), 
           tree_depth = 10, 
           min_n = 1)


## Create a regular tune grid ---------------------------------------------

xgboost_grid <- grid_regular(range_set(mtry(), c(4, 8)),
                             range_set(trees(), c(500, 1000)),
                             range_set(learn_rate(trans = NULL), c(0.01, 0.02)),
                             # define the levels
                             levels = 3)

## Create model workflow --------------------------------------------------

xgboost_wflow <- 
  workflow() %>% 
  add_model(xgboost_model) %>% 
  add_recipe(df_recipe)



## Analyse resamples with hyperparameter tuning ---------------------------

system.time({
  set.seed(1234)
  xgboost_results <- xgboost_wflow %>% 
    tune_grid(resamples = train_ML_folds, 
              grid = xgboost_grid, 
              control = control_grid(verbose = TRUE))
})


autoplot(xgboost_results)


show_best(x = xgboost_results,
          metric = "accuracy")
  


## Finalise model workflow ------------------------------------------------

best_xgboost <- select_best(x = xgboost_results, 
                            metric = "accuracy")

final_wf <- 
  xgboost_wflow %>% 
  finalize_workflow(best_xgboost)

final_xgboost <- fit(final_wf, train_ML)




# predict test data -------------------------------------------------------

test_ML <- test %>%   
  # remove identifiers
  select(-c(PassengerId, Cabin, Name)) %>% 
  # make sure the target variable is a class (binary)
  mutate(CryoSleep = as.factor(CryoSleep),
         VIP = as.factor(VIP))
  

xgboost_pred_prob <- predict(final_xgboost, 
                             new_data = test_ML, 
                             type = "prob")
xgboost_pred_class <- predict(final_xgboost, 
                              new_data = test_ML, 
                              type = "class")

# Data frame from test set with the model predictions “attached”
predictions <-  test %>% 
  bind_cols(., xgboost_pred_prob, xgboost_pred_class)


predictions %>% 
  select(PassengerId, .pred_class) %>% 
  rename("Transported" = .pred_class) %>% 
  # Needs to be exactly as in the description!
  mutate(Transported = str_to_title(Transported)) %>% 
  write_csv(file = "ML_Monday_scripts/sample_submission.csv")


# Stop parallelisation ----------------------------------------------------
parallel::stopCluster(cl)
closeAllConnections()
