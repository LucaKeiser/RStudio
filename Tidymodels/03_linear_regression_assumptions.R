
# linear regression analysis ----------------------------------------------





# load packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)





# load data ---------------------------------------------------------------
penguins

theme_set(theme_minimal())




# EDA ---------------------------------------------------------------------
penguins %>% 
  ggplot(aes(body_mass_g)) + 
  geom_histogram(bins = 15) + 
  scale_x_log10()

penguins %>% 
  ggplot(aes(flipper_length_mm, body_mass_g)) + 
  geom_point()

penguins %>% 
  ggplot(aes(bill_length_mm, body_mass_g)) + 
  geom_point()





# test model assumptions --------------------------------------------------

## 1. test linearity ------------------------------------------------------
residuals <- tibble(
  res_body_mass_g_flipper_length_mm = residuals(lm(body_mass_g ~ flipper_length_mm, 
                                                   data = penguins)),
  res_bill_length_mm_flipper_length_mm = residuals(lm(bill_length_mm ~ flipper_length_mm, 
                                                      data = penguins)),
  res_body_mass_g_bill_length_mm = residuals(lm(body_mass_g ~ bill_length_mm, 
                                                data = penguins)),
  res_flipper_length_mm_bill_length_mm = residuals(lm(flipper_length_mm ~ bill_length_mm, 
                                                      data = penguins))
)

residuals %>% 
  ggplot(aes(res_bill_length_mm_flipper_length_mm, res_body_mass_g_flipper_length_mm)) + 
  geom_point() + 
  labs(title = "Linearity could be better...")


residuals %>% 
  ggplot(aes(res_flipper_length_mm_bill_length_mm, res_body_mass_g_bill_length_mm)) + 
  geom_point() + 
  labs(title = "Looks good!")



## 2. create model --------------------------------------------------------
linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

linear_recipe <- recipe(body_mass_g ~ flipper_length_mm  + bill_length_mm,
                        data = penguins) %>% 
  step_normalize(all_numeric_predictors())

linear_workflow <- workflow() %>% 
  add_model(linear_model) %>% 
  add_recipe(linear_recipe)

model_output <- linear_workflow %>% 
  fit(penguins) %>% 
  extract_fit_engine() 

summary(model_output)



## 3. test tolerance ------------------------------------------------------
1/car::vif(model_output)
# => everything **above** 0.2 is okay!



## 4. test homoscedasticity/variance homogeneity --------------------------
tibble(
  fitted_values = fitted(model_output),
  residuals = residuals(model_output)
) %>% 
  ggplot(aes(fitted_values, residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0,
             color = "red") + 
  labs(title = "Looks alright!")
# => horizontal spread of the values should be about the same
# => does not look too bad here...



## 5. normal distributed residuals ----------------------------------------
plot_df <- tibble(
  residuals = residuals(model_output)
)

plot_df %>% 
  ggplot() + 
  geom_histogram(aes(residuals, y = after_stat(density)),
                 color = "white") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(plot_df$residuals), 
                            sd = sd(plot_df$residuals)),
                color = "red",
                lty = 2,
                linewidth = 2) +
  labs(title = "Looks very good!")





# interpret the model -----------------------------------------------------

summary(model_output)

model_output %>% 
  summary() %>% 
  broom::tidy(conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(estimate, term)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.1) + 
  geom_vline(xintercept = 0,
             color = "red",
             lty = 2) + 
  expand_limits(x = c(-100, 800))

model_output %>% 
  anova() %>% 
  broom::tidy() %>% 
  mutate(pct_explained = sumsq /sum(sumsq)) %>% 
  mutate(term = fct_reorder(term, pct_explained)) %>% 
  ggplot(aes(pct_explained, term)) + 
  geom_col(aes(fill = term != "Residuals"),
           show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent_format())
