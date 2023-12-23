
# linear regression analysis ----------------------------------------------





# load packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(modelr)
theme_set(theme_minimal())





# load data ---------------------------------------------------------------
penguins





# EDA ---------------------------------------------------------------------
penguins %>% 
  skimr::skim()

penguins <- penguins %>% 
  filter(!is.na(sex))

penguins %>% 
  count(sex) %>% 
  mutate(sex = fct_reorder(sex, n)) %>% 
  ggplot(aes(sex, n, fill = sex)) +
  geom_col(show.legend = FALSE) 

penguins %>% 
  ggplot(aes(body_mass_g)) + 
  geom_density(aes(fill = sex),
               alpha = 0.5) +
  facet_wrap(~ species) + 
  scale_x_log10()

penguins %>% 
  ggplot(aes(flipper_length_mm)) + 
  geom_density(aes(fill = sex),
               alpha = 0.5) +
  facet_wrap(~ species)





# create model ------------------------------------------------------------
logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

logistic_recipe <- recipe(sex ~ body_mass_g + flipper_length_mm + species,
                          data = penguins) %>%
  step_log(body_mass_g, base = 2)

logistic_recipe %>% 
  prep() %>% 
  juice()

logistic_workflow <- workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(logistic_recipe)

model_output <- logistic_workflow %>% 
  fit(penguins) %>% 
  extract_fit_engine()

summary(model_output)
descr::LogRegR2(model_output)
# => Nagelkerke Index (model fit)
#    values above 0.5 are good, so good model fit here.

# => McFadden's R^2 (relative improvement) 
#    compared to the 0-model (no independent variables) the 1-model 
#    (independent variables) the explanatory power of the model 
#    improves by 53.77%



## test multicollinearity -------------------------------------------------
car::vif(model_output)
# => GVIF^(1/(2*Df)) should be below 2...
# => multicollinearity is a problem (body_mass_g and flipper_length_mm)
penguins %>% 
  select(body_mass_g, flipper_length_mm, species) %>%
  mutate(species = as.numeric(species)) %>% 
  cor()





# create 2. model ---------------------------------------------------------
# remove flipper_lenght_mm to reduce multicollinearity
logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

logistic_recipe_2 <- recipe(sex ~ body_mass_g + species,
                            data = penguins) %>%
  step_log(body_mass_g, base = 2) %>% 
  step_mutate(sex = factor(ifelse(sex == "female", 1, 0)))

logistic_recipe_2 %>% 
  prep() %>% 
  juice()

logistic_workflow_2 <- workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(logistic_recipe_2)

model_output_2 <- logistic_workflow_2 %>% 
  fit(penguins) %>% 
  extract_fit_engine()

summary(model_output_2)
descr::LogRegR2(model_output_2)
# => Nagelkerke Index (model fit)
#    values above 0.5 are good, so good model fit here.

# => McFadden's R^2 (relative improvement) 
#    compared to the 0-model (no independent variables) the 1-model 
#    (independent variables) the explanatory power of the model 
#    improves by 52.33%

# => NOTE: model is still very good. No need to keep flipper_length_mm...



## test multicollinearity -------------------------------------------------
car::vif(model_output_2)
# => GVIF^(1/(2*Df)) should be below 2...
# => multicollinearity is still a problem (body_mass_g and species)
penguins %>% 
  select(body_mass_g, species) %>%
  mutate(species = as.numeric(species)) %>% 
  cor()





# create 3. model ---------------------------------------------------------
# remove species to reduce multicollinearity
logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

logistic_recipe_3 <- recipe(sex ~ body_mass_g,
                            data = penguins) %>%
  step_log(body_mass_g, base = 2) %>% 
  step_mutate(sex = factor(ifelse(sex == "female", 1, 0)))

logistic_recipe_3 %>% 
  prep() %>% 
  juice()

logistic_workflow_3 <- workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(logistic_recipe_3)

model_output_3 <- logistic_workflow_3 %>% 
  fit(penguins) %>% 
  extract_fit_engine()

summary(model_output_3)
descr::LogRegR2(model_output_3)
# => Nagelkerke Index (model fit)
#    values above 0.5 are good. Model does not fit the data well...

# => McFadden's R^2 (relative improvement) 
#    compared to the 0-model (no independent variables) the 1-model 
#    (independent variables) the explanatory power of the model 
#    improves by 14.50%. 

# => NOTE: model does not explain the variance within the data...
#          Under this circumstances the 2. model performs best...





# interpret the model -----------------------------------------------------
penguins_fitted <- penguins %>% 
  select(sex, body_mass_g, species) %>% 
  # same transformation is needed!
  mutate(body_mass_g = log2(body_mass_g),
         sex = factor(ifelse(sex == "female", 1, 0))) %>% 
  add_predictions(model_output_2,
                  type = "response") %>% 
  mutate(sex_pred = factor(ifelse(pred >= 0.5, 1, 0)))


conf_mat(penguins_fitted,
         truth = sex, 
         estimate = sex_pred)


penguins_fitted %>% 
  add_count(sex, sex_pred) %>% 
  ggplot() +
  geom_jitter(aes(sex_pred, sex,
                  color = sex == sex_pred),
              height = 0.25,
              width = 0.25) + 
  labs(color = "Right prediced?") + 
  annotate(geom = "label",
           x = 1, y = 2,
           label = "n = 23") + 
  annotate(geom = "label",
           x = 2, y = 2,
           label = "n = 142") + 
  annotate(geom = "label",
           x = 1, y = 1,
           label = "n = 142") + 
  annotate(geom = "label",
           x = 2, y = 1,
           label = "n = 26")

