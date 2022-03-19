

####################### Get started with tidymodels using vaccination rate data ####################### 

# source: https://www.youtube.com/watch?v=E2Ld3QdXYZo


# packages ----------------------------------------------------------------

library(tidyverse)
library(tidymodels)



# get the data ------------------------------------------------------------

measles <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv")
readr::spec(measles)




# take a look at the mmr per state ----------------------------------------

# create df
measles_df <- measles %>% 
  filter(mmr > 0) %>% 
  transmute(state,
            mmr_threshold = case_when(mmr > 95 ~ "Above",
                                      TRUE ~ "Below")) %>% 
  mutate_if(is.character, factor)

# plot
measles_df %>% 
  group_by(state) %>% 
  summarise(mmr = mean(mmr_threshold == "Above")) %>% 
  mutate(state = fct_reorder(state, mmr)) %>% 
  ggplot(aes(state, mmr, fill = state)) + 
  geom_col(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  labs(title = "What is the MMR-rate of US-states?",
       subtitle = "MMR: School's Measles, Mumps, and Rubella vaccination rate",
       x = "",
       y = "",
       caption = "")



# tidymodels --------------------------------------------------------------

### GLM
# creating a classification model => which schools are above and below the threshold?
# train a pretty basic glm-model

glm_fit <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(mmr_threshold ~ state, 
      data = measles_df)

# take a look at the results
glm_fit
tidy(glm_fit)

# which states are significantly different from the "base state" (= Arizona, the first in an 
# alphabetical order...)?
tidy(glm_fit) %>% 
  filter(p.value < 0.05)

# problem with glm-models => it's all on a logistic scale => not intuitive to interpret...
# moreover, it is not easy to get relevant information out of the fitted model with this 
# conventional approach...

# => tidymodels can help here!



### Predict with tidymodels!
# let's make some predictions with tidymodel => much easier!!!

# create a new df with our predictions
# with the tidymodels package you can use the same approach for ever model!
# create df with the state names
new_schools <- tibble(state = unique(measles_df$state))

# get the mean predictions
mean_pred <- predict(glm_fit,
                     new_data = new_schools,
                     type = "prob")

# get the confidence intervals for the predictions!
conf_int <- predict(glm_fit, 
                    new_data = new_schools, 
                    type = "conf_int", 
                    level = 0.95)

# bind cols
schools_results <- new_schools %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int) %>% 
  janitor::clean_names()


# visualize
schools_results %>% 
  mutate(state = fct_reorder(state, pred_above)) %>% 
  ggplot(aes(state, pred_above, fill = state)) +
  geom_col(show.legend = FALSE) + 
  geom_errorbar(aes(ymin = pred_lower_above,
                    ymax = pred_upper_above),
                color = "gray30") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() + 
  labs(title = "What is the probability for a state to be above an MMR-threshold of 95%",
       subtitle = "MMR: School's Measles, Mumps, and Rubella vaccination rate",
       x = "",
       y = "",
       caption = "Confidence interval: 95%")
  
# the larger the span of the error bar the more uncertain is our prediction




# Bayesian model ----------------------------------------------------------

# what if the glm is not good enough?
# let's fit an Bayesian model

library(rstanarm)

# parallel processing
ncores <- parallel::detectCores()
ctemp <- parallel::makeCluster(ncores-1)
doSNOW::registerDoSNOW(ctemp)


# specify the model (not exact...)
prior_dist <- student_t(2)

stan_fit <- logistic_reg() %>% 
  set_engine("stan",
             prior = prior_dist,
             prior_intercept = prior_dist) %>% 
  fit(mmr_threshold ~ state, 
      data = measles_df)


# take a look at the results
stan_fit

bayes_pred <- predict(stan_fit,
                      new_data = new_schools,
                      type = "prob")

bayes_int <- predict(stan_fit,
                     new_data = new_schools,
                     # in Bayesian statistics these are called credible intervals...
                     type = "conf_int",
                     level = 0.95)


bayes_results <- new_schools %>% 
  bind_cols(bayes_pred) %>% 
  bind_cols(bayes_int) %>% 
  janitor::clean_names()




# compare the results -----------------------------------------------------

schools_results %>% 
  mutate(model = "glm") %>% 
  bind_rows(bayes_results %>% 
              mutate(model = "stan")) %>% 
  mutate(state = fct_reorder(state, pred_above)) %>% 
  ggplot(aes(state, pred_above, color = model)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = pred_lower_above,
                    ymax = pred_upper_above),
                alpha = 0.5,
                size = 1) +
  coord_flip() +
  scale_color_discrete(labels = c("GLM", "Stan")) + 
  labs(title = "Model Comparison - What is the probability for a state to be above an MMR-threshold of 95%",
       subtitle = "MMR: School's Measles, Mumps, and Rubella vaccination rate",
       x = "",
       y = "",
       caption = "Confidence interval: 95%",
       color = "Model")


# models are very close to each other! Priors are exact...



