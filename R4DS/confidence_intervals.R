
# load packages and create data set ---------------------------------------

library(tidyverse)
library(rsample)
library(glue)

theme_set(theme_minimal())

set.seed(1234)
df_original <- tibble(
  variable_of_interest = rnorm(n = 1000, 
                               mean = 5, 
                               sd = 1.25)
) %>% 
  mutate(variable_of_interest = round(variable_of_interest))


# confidence intervals ----------------------------------------------------

## t-distribution ---------------------------------------------------------

df_temp <- tibble(n_obs = c(10, 20, 30, 40, 50, 100, 
                            200, 500, 750, 1e3, 1e4, 1e5)) %>% 
  mutate(mean_value = mean(df_original$variable_of_interest),
         sd_value = sd(df_original$variable_of_interest),
         conf_low = mean_value - qt(0.975, df = n_obs - 1) * sd_value / sqrt(n_obs),
         conf_high = mean_value + qt(0.975, df = n_obs - 1) * sd_value / sqrt(n_obs))

df_temp %>% 
  ggplot(aes(mean_value, factor(n_obs))) + 
  geom_label(x = 5.75, y = 10,
             size = 5,
             label = glue("Calculated cample mean: {round(mean(df_temp$mean_value), 2)}")) + 
  geom_point(aes(color = factor(n_obs)),
             size = 3) + 
  geom_errorbarh(aes(xmin = conf_low,
                     xmax = conf_high,
                     color = factor(n_obs)),
                 linewidth = 1,
                 height = 0.5) +
  expand_limits(x = c(4, 6)) +
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(4, 6, 0.25)) +
  labs(x = "\nCalculated sample mean and 95% confidence interval\n",
       y = "\nSample size (number of observations)\n")


## bootstraps -------------------------------------------------------------

make_bootstraps <- function(times = times) {
  
  set.seed(1234)
  
  df_original %>% 
    select(variable_of_interest) %>% 
    bootstraps(times = times) %>% 
    mutate(splits = map(splits, as.data.frame)) %>% 
    unnest(splits) %>% 
    group_by(id) %>% 
    summarise(mean_splits = mean(variable_of_interest)) %>% 
    ungroup() %>% 
    summarise(conf_low = quantile(mean_splits, 0.025),
              conf_high = quantile(mean_splits, 0.975)) %>% 
    mutate(n_bootstraps = times,
           mean_value = mean(df_original$variable_of_interest))
  
}


tibble(
  n_runs = c(10, 20, 30, 40, 50, 100, 
             200, 500, 750, 1e3)
) %>% 
  mutate(bootstrapped_value = map(n_runs, make_bootstraps)) %>% 
  unnest(bootstrapped_value) %>% 
  ggplot(aes(mean_value, factor(n_runs))) + 
  geom_label(x = 5.75, y = 8,
             size = 5,
             label = glue("Stichprobenmittelwert: {round(mean(df_original$variable_of_interest), 2)}")) + 
  geom_point(aes(color = factor(n_bootstraps)),
             size = 3) + 
  geom_errorbarh(aes(xmin = conf_low,
                     xmax = conf_high,
                     color = factor(n_bootstraps)),
                 linewidth = 1,
                 height = 0.5) +
  expand_limits(x = c(4, 6)) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(4, 6, 0.25)) +
  labs(x = "\nCalculated sample mean and bootstrapped intervals\n",
       y = "\nNumber of bootstrap runs\n")


