library(tidyverse)
library(tidymodels)
library(modelr)
library(broom)

theme_set(theme_minimal())


# binary linear regression ------------------------------------------------

# set seed
set.seed(1234)

# generate exogenous variables
df <- tibble(
  x = rnorm(n = 5000, 
            mean = 0,
            sd = 1),
  e = rnorm(n = 5000,
            mean = 0, 
            sd = 1)
)

# take a look at the distributions
df %>% 
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "value") %>% 
  ggplot(aes(value, 
             fill = name,
             color = name)) + 
  geom_density(alpha = 0.2)

# population parameters
b0 <- 0.3
b1 <- 0.7


# generate endogenous variable
df <- df %>% 
  mutate(y = b0 + b1 * x + e) %>% 
  relocate(y, x, e)

# take a look at the distribution as well
df %>% 
  ggplot(aes(y)) + 
  geom_density(fill = "red",
               alpha = 0.5)


# relationship between y and y
df %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE)
cor(df$x, df$y)

# => good enough for a linear model!



# linear model with tidymodels

lm_model <- linear_reg() %>% 
  set_engine("lm")

lm_recipe <- recipe(y ~ x, data = df)

lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(lm_recipe)

lm_wflow

# fit
lm_fit <- fit(lm_wflow, df)

# check model quality
df <- df %>% 
  mutate(pred = predict(lm_fit,
                        new_data = df)$.pred,
         resid = y - pred)

# plot 1
df %>%
  ggplot(aes(pred, resid)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm",
              se = FALSE) + 
  geom_hline(yintercept = 0, 
             lty = 2, 
             color = "red")

# plot 2
df %>% 
  ggplot() + 
  geom_point(aes(x, y),
             alpha = 0.2) +
  geom_line(aes(x, pred),
            color = "red")

# metrics
lm_fit %>% 
  extract_fit_engine() %>% 
  summary()

quality_metrics <- metric_set(rmse, rsq, mae)
quality_metrics(df, 
                truth = y, 
                estimate = pred)

# compare coeffients to true values
# true values
c(b0, b1)

# coefficients
lm_fit %>% 
  extract_fit_engine() %>% 
  summary() %>% 
  coef()

lm_fit %>% 
  broom::tidy(conf.int = TRUE,
              conf.level = 0.95) %>% 
  ggplot() + 
  geom_point(aes(estimate, term)) + 
  geom_errorbar(aes(estimate, term,
                    xmin = conf.low, 
                    xmax = conf.high),
                width = 0.1) + 
  geom_vline(xintercept = 0,
             lty = 2,
             color = "red")



# mutliple linear regression ----------------------------------------------

# exogenous variables
set.seed(1234)

df_2 <- tibble(
  x1 = rnorm(n = 1000,
             mean = 0, 
             sd = 15),
  x2 = rnorm(n = 1000,
             mean = -10,
             sd = 0.5),
  e = rnorm(n = 1000,
            mean = 0, 
            sd = 1)
)

df_2 %>% 
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "value") %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = 0.2)


# population parameter
b0 <- -10
b1 <- 0.1
b2 <- 10

# endogenous variable
df_2 <- df_2 %>% 
  mutate(y = b0 + b1 * x1 + b2 * x2 + e)

# take a look at the distribution as well
df_2 %>% 
  ggplot(aes(y)) + 
  geom_density(fill = "red",
               alpha = 0.5)


# create models (not with tidymodels)
m_lm_1 <- lm(y ~ x1, data = df_2)
m_lm_2 <- lm(y ~ x2, data = df_2)
m_lm_3 <- lm(y ~ x1 + x2, data = df_2)



# model qualitiy

# plot 1
gridExtra::grid.arrange(
  
  df_2 %>% 
    add_predictions(m_lm_1) %>% 
    add_residuals(m_lm_1) %>% 
    ggplot(aes(pred, y)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod1"),
  
  df_2 %>% 
    add_predictions(m_lm_2) %>% 
    add_residuals(m_lm_2) %>% 
    ggplot(aes(pred, y)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod2"),
  
  df_2 %>% 
    add_predictions(m_lm_3) %>% 
    add_residuals(m_lm_3) %>% 
    ggplot(aes(pred, y)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod3"),
  
  ncol = 3
)

# plot 2
gridExtra::grid.arrange(
  
  df_2 %>% 
    add_predictions(m_lm_1) %>% 
    add_residuals(m_lm_1) %>% 
    ggplot(aes(pred, resid)) + 
    geom_point() + 
    geom_hline(yintercept = 0,
               color = "red",
               size = 1.5) +  
    expand_limits(x = c(-115, -105),
                  y = c(-15, 15)) + 
    labs(title = "mod1"),
  
  df_2 %>% 
    add_predictions(m_lm_2) %>% 
    add_residuals(m_lm_2) %>% 
    ggplot(aes(pred, resid)) + 
    geom_point() + 
    geom_hline(yintercept = 0,
               color = "red",
               size = 1.5) + 
    expand_limits(x = c(-115, -105),
                  y = c(-15, 15)) + 
    labs(title = "mod2"),
  
  df_2 %>% 
    add_predictions(m_lm_3) %>% 
    add_residuals(m_lm_3) %>% 
    ggplot(aes(pred, resid)) + 
    geom_point() + 
    geom_hline(yintercept = 0,
               color = "red",
               size = 1.5) +
    expand_limits(x = c(-115, -105),
                  y = c(-15, 15)) + 
    labs(title = "mod3"),
  
  ncol = 3
)



# metrics
c(b0, b1, b2)
stargazer::stargazer(m_lm_1,
                     m_lm_2,
                     m_lm_3,
                     type = "text")

# vip
pct_variation <- function(model) {
  model %>% 
    anova() %>% 
    tidy() %>% 
    mutate(pct_variation = sumsq/sum(sumsq))
}


pct_variation(m_lm_1)
pct_variation(m_lm_2)
pct_variation(m_lm_3)




