library(tidyverse)
library(modelr)
library(tidymodels)
library(broom)
library(splines)
library(scales)
library(lubridate)

options(na.action = na.warn)
theme_set(theme_minimal())

library(gapminder)



# funciton ----------------------------------------------------------------
pct_variation <- function(model) {
  model %>% 
    anova() %>% 
    tidy() %>% 
    mutate(pct_variation = sumsq/sum(sumsq))
}



# get data ----------------------------------------------------------------
set.seed(1234)

initail_split <- initial_split(gapminder, strata = year)

gapminder_training <- training(initail_split) %>% 
  arrange(year) %>% 
  rowid_to_column()

gapminder_testing <- testing(initail_split) %>% 
  arrange(year) %>% 
  rowid_to_column()

skimr::skim(gapminder_training)
skimr::skim(gapminder_testing)



# EDA ---------------------------------------------------------------------

## overall ----------------------------------------------------------------
gapminder_training %>% 
  group_by(year) %>% 
  summarise(overall_mean_lifeExp = mean(lifeExp)) %>% 
  ggplot(aes(year, overall_mean_lifeExp)) + 
  geom_line() + 
  geom_label(aes(label = round(overall_mean_lifeExp, 2))) + 
  expand_limits(y = 0) +
  labs(title = "Average life expectancy over time",
       subtitle = "Shown is the overall average value per year",
       x = "",
       y = "Average life expectancy",
       caption = "Data: gapminder (142 countries)")

gapminder_training %>% 
  group_by(year) %>% 
  summarise(overall_mean_gdpPercap = mean(gdpPercap),
            overall_median_gdpPercap = median(gdpPercap)) %>% 
  pivot_longer(cols = c(overall_mean_gdpPercap, overall_median_gdpPercap),
               names_to = "type",
               values_to = "value") %>%  
  ggplot(aes(year, value, color = type)) + 
  geom_line() +
  geom_point() + 
  expand_limits(y = 0) +
  scale_color_discrete(name = "Type:", 
                       labels = c("Mean", "Median")) + 
  labs(title = "GDP per capita over time",
       subtitle = "Shown are the overall average values (mean and median) per year",
       x = "",
       y = "Average GDP per Capita",
       caption = "Data: gapminder (142 countries)")


gapminder_training %>% 
  group_by(year) %>% 
  summarise(overall_pop = sum(pop)) %>% 
  ggplot(aes(year, overall_pop)) + 
  geom_line(color = "#F8766D") + 
  geom_point(color = "#F8766D") + 
  scale_y_continuous(breaks = seq(0, 5000000000, 1000000000),
                     labels = c("0 Billion", "1 Billion", "2 Billions", 
                                "3 Billions", "4 Billions", "5 Billions")) +
  labs(title = "Population over time",
       subtitle = "Shown is the overall sum per year",
       x = "",
       y = "Population",
       caption = "Data: gapminder (142 countries)\nShape looks a bit weird...")




## per continent ----------------------------------------------------------
gapminder_training %>% 
  group_by(year, continent) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  mutate(continent = fct_reorder(continent, -mean_lifeExp)) %>% 
  ggplot(aes(year, mean_lifeExp, color = continent)) + 
  geom_line() + 
  geom_label(aes(label = round(mean_lifeExp, 2))) + 
  expand_limits(y = 0) +
  labs(title = "Average life expectancy per continent and over time",
       x = "",
       y = "Average life expectancy",
       color = "Continent:",
       caption = "Data: gapminder (142 countries)")


gapminder_training %>% 
  group_by(year, continent) %>% 
  summarise(overall_mean_gdpPercap = mean(gdpPercap),
            overall_median_gdpPercap = median(gdpPercap)) %>% 
  pivot_longer(cols = c(overall_mean_gdpPercap, overall_median_gdpPercap),
               names_to = "type",
               values_to = "value") %>%
  mutate(continent = fct_reorder(continent, -value)) %>% 
  ggplot(aes(year, value, color = continent, linetype = type)) + 
  geom_line() +
  geom_point() + 
  scale_linetype_discrete(name = "Type:", 
                          labels = c("Mean", "Median")) +
  scale_y_continuous(breaks = seq(0, 30000, 5000),
                     labels = comma_format(big.mark = "`")) + 
  labs(title = "GDP per capita over time",
       subtitle = "Shown are the average values (mean and median) per year and continent",
       color = "Continent:",
       x = "",
       y = "Average GDP per Capita",
       caption = "Data: gapminder (142 countries)")


gapminder_training %>% 
  group_by(year, continent) %>% 
  summarise(overall_pop = sum(pop)) %>% 
  mutate(continent = fct_reorder(continent, -overall_pop)) %>% 
  ggplot(aes(year, overall_pop, color = continent)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(breaks = seq(0, 3000000000, 1000000000),
                     labels = c("0 Billion", "1 Billion", "2 Billions", "3 Billions")) + 
  labs(title = "Population over time",
       subtitle = "Per continent",
       x = "",
       y = "Average population",
       color = "Continent:",
       caption = "Data: gapminder (142 countries)\nData for Asia might be wrong...")




# what predicts lifeExp? --------------------------------------------------

gapminder_training %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_jitter() + 
  geom_smooth(method = "lm",
              se = FALSE)


gapminder_training %>% 
  ggplot(aes(pop, lifeExp)) + 
  geom_jitter() +
  geom_smooth(method = "lm",
              se = FALSE) + 
  scale_x_log10()


gapminder_training %>% 
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_jitter() + 
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10()


gapminder_training %>% 
  mutate(continent = fct_reorder(continent, -lifeExp)) %>% 
  ggplot(aes(continent, lifeExp, fill = continent)) +
  geom_boxplot()



# mod1 - continent only --------------------------------------------------
mod1 <- lm(lifeExp ~ continent, 
           data = gapminder_training)

summary(mod1)

mod1 %>% 
  pct_variation()

gapminder_training %>% 
  add_predictions(mod1) %>% 
  add_residuals(mod1) %>%
  relocate(lifeExp, .after = gdpPercap) %>% 
  View()

gapminder_training %>% 
  add_residuals(mod1) %>% 
  ggplot() + 
  geom_point(aes(rowid, resid, color = continent)) + 
  geom_smooth(aes(rowid, resid, color = continent),
              method = "lm",
              se = FALSE) +
  geom_hline(yintercept = 0,
             lty = 2)

gapminder_training %>% 
  add_residuals(mod1) %>% 
  ggplot(aes(continent, resid, fill = continent)) + 
  geom_boxplot(show.legend = FALSE,
               outlier.alpha = 0.5) +
  geom_hline(yintercept = 0,
             lty = 2,
             color = "grey50")

gapminder_training %>% 
  add_predictions(mod1) %>% 
  add_residuals(mod1) %>%
  pivot_longer(cols = c(lifeExp, pred),
               names_to = "type", 
               values_to = "values") %>%
  ggplot() + 
  geom_point(aes(rowid, values, color = continent)) + 
  facet_wrap(~ type)


gapminder_training %>% 
  add_predictions(mod1) %>% 
  add_residuals(mod1) %>% 
  ggplot(aes(pred, lifeExp)) + 
  geom_point(aes(color = continent)) + 
  geom_smooth(method = "lm",
              se = FALSE)



# mod2 - add year ---------------------------------------------------------

mod2 <- lm(lifeExp ~ continent + year,
           data = gapminder_training)

summary(mod2)

mod2 %>% 
  pct_variation()


gapminder_training %>% 
  spread_predictions("mod1_pred" = mod1, 
                     "mod2_pred" = mod2) %>% 
  spread_residuals("mod1_resid" = mod1,
                   "mod2_resid" = mod2) %>% 
  View()


gapminder_training %>% 
  gather_residuals(mod1, mod2) %>% 
  ggplot(aes(rowid, resid, color = model)) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(se = FALSE) + 
  geom_hline(yintercept = 0, 
             lty = 2)


gapminder_training %>% 
  spread_predictions("mod1_pred" = mod1, 
                     "mod2_pred" = mod2) %>% 
  spread_residuals("mod1_resid" = mod1,
                   "mod2_resid" = mod2) %>%
  mutate(resid_abs_diff = abs(mod2_resid) - abs(mod1_resid),
         performance = ifelse(resid_abs_diff < 0, "better", "worse"),
         rowid = fct_reorder(factor(rowid), resid_abs_diff)) %>%
  ggplot(aes(rowid, resid_abs_diff, fill = performance)) + 
  geom_col() +
  scale_fill_manual(values = c("better" = "darkgreen",
                               "worse" = "red")) + 
  theme(axis.text.x = element_text(angle = 90,
                                   size = 0.5)) + 
  labs(title = "mod2 vs. mod1")


gapminder_training %>% 
  spread_predictions("mod1_pred" = mod1, 
                     "mod2_pred" = mod2) %>% 
  spread_residuals("mod1_resid" = mod1,
                   "mod2_resid" = mod2) %>%
  mutate(resid_abs_diff = abs(mod2_resid) - abs(mod1_resid),
         performance = ifelse(resid_abs_diff < 0, "better", "worse")) %>% 
  group_by(performance) %>% 
  summarise(sum_resid_abs_diff = sum(resid_abs_diff))

gridExtra::grid.arrange(
  
  gapminder_training %>% 
    data_grid(continent, year) %>% 
    mutate(pred = predict(mod1, .)) %>% 
    ggplot() +
    geom_line(aes(year, pred, color = continent)) + 
    labs(title = "mod1 - only \"continent\" as predictor"),
  
  gapminder_training %>% 
    data_grid(continent, year) %>% 
    mutate(pred = predict(mod2, .)) %>% 
    ggplot() +
    geom_line(aes(year, pred, color = continent)) + 
    labs(title = "mod2 - \"year\" as predictor added"),
  
  ncol = 2
  
)

gridExtra::grid.arrange(
  
  mod1 %>% 
    tidy(conf.int = TRUE, 
         conf.level = 0.95) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(term = fct_reorder(term, estimate)) %>% 
    ggplot(aes(estimate, term)) + 
    geom_point() + 
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  size = 1) +
    geom_vline(xintercept = 0,
               color = "red",
               lty = 2,
               size = 1) +
    expand_limits(x = c(-5, 30)) + 
    labs(title = "mod1 - only \"continent\" as predictor"),
  
  mod2 %>% 
    tidy(conf.int = TRUE, 
         conf.level = 0.95) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(term = fct_reorder(term, estimate)) %>% 
    ggplot(aes(estimate, term)) + 
    geom_point() + 
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  size = 1) +
    geom_vline(xintercept = 0,
               color = "red",
               lty = 2,
               size = 1) +
    expand_limits(x = c(-5, 30)) + 
    labs(title = "mod2 - \"year\" as predictor added"),
  
  ncol = 2
  
)



gapminder_training %>% 
  add_predictions(mod2) %>% 
  add_residuals(mod2) %>% 
  ggplot(aes(pred, lifeExp)) + 
  geom_point() + 
  geom_abline(color = "red")



# mod3 - add gdpPercap ----------------------------------------------------
mod3 <- lm(lifeExp ~ continent + year + log2(gdpPercap), data = gapminder_training)

summary(mod3)

mod3 %>% 
  pct_variation()

gapminder_training %>% 
  gather_residuals(mod1, mod2, mod3) %>% 
  ggplot(aes(rowid, resid, color = model)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0,
             lty = 2)


gapminder_training %>% 
  spread_predictions("mod1_pred" = mod1, 
                     "mod2_pred" = mod2,
                     "mod3_pred" = mod3) %>% 
  spread_residuals("mod1_resid" = mod1,
                   "mod2_resid" = mod2,
                   "mod3_resid" = mod3) %>% 
  View()

gridExtra::grid.arrange(
  
  gapminder_training %>% 
    data_grid(continent, year) %>% 
    mutate(pred = predict(mod1, .)) %>% 
    ggplot() +
    geom_line(aes(year, pred, color = continent)) + 
    labs(title = "mod1 - only \"continent\" as predictor"),
  
  gapminder_training %>% 
    data_grid(continent, year) %>% 
    mutate(pred = predict(mod2, .)) %>% 
    ggplot() +
    geom_line(aes(year, pred, color = continent)) + 
    labs(title = "mod2 - \"year\" as predictor added"),
  
  gapminder_training %>% 
    mutate(pred = predict(mod3, .)) %>% 
    ggplot() +
    geom_jitter(aes(year, pred, color = continent, group = continent)) + 
    labs(title = "mod3 - \"gdpPercap\" as predictor added"),
  
  ncol = 3
  
)


gapminder_training %>% 
  spread_predictions("mod2_pred" = mod2, 
                     "mod3_pred" = mod3) %>% 
  spread_residuals("mod2_resid" = mod2,
                   "mod3_resid" = mod3) %>%
  mutate(resid_abs_diff = abs(mod3_resid) - abs(mod2_resid),
         performance = ifelse(resid_abs_diff < 0, "better", "worse"),
         rowid = fct_reorder(factor(rowid), resid_abs_diff)) %>% 
  ggplot(aes(rowid, resid_abs_diff, fill = performance)) + 
  geom_col() +
  scale_fill_manual(values = c("better" = "darkgreen",
                               "worse" = "red")) + 
  theme(axis.text.x = element_blank()) + 
  labs(title = "mod3 vs. mod2")


gapminder_training %>% 
  spread_predictions("mod2_pred" = mod2, 
                     "mod3_pred" = mod3) %>% 
  spread_residuals("mod2_resid" = mod2,
                   "mod3_resid" = mod3) %>%
  mutate(resid_abs_diff = abs(mod3_resid) - abs(mod2_resid),
         performance = ifelse(resid_abs_diff < 0, "better", "worse")) %>% 
  group_by(performance) %>% 
  summarise(sum_resi = sum(resid_abs_diff))



gapminder_training %>% 
  add_predictions(mod3) %>% 
  add_residuals(mod3) %>% 
  ggplot(aes(pred, lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              se = FALSE)




# mod4 - add new variable -------------------------------------------------

# get data
regions_json <- jsonlite::read_json("https://gist.githubusercontent.com/hvva/c6b3b4faa8a7c7bd9751/raw/be8e3591825341880e22cfa5211f2bcf34d3f3fb/UNCountries.json",
                                    full.names = TRUE)

# convert
regions <- tibble(json = regions_json) %>% 
  hoist(json, region = "region") %>% 
  hoist(json, subregion = "subregion") %>% 
  hoist(json, iso3 = "iso3") %>% 
  hoist(json, country = "country")


# join - check first...
gapminder_training %>% 
  anti_join(gapminder::country_codes, 
            by = "country")
gapminder_training <- gapminder_training %>% 
  left_join(gapminder::country_codes,
            by = "country")

gapminder_training %>% 
  anti_join(regions, 
            by = c("iso_alpha" = "iso3"))
gapminder_training <- gapminder_training %>% 
  left_join(regions,
            by = c("iso_alpha" = "iso3"))

# good!
gapminder_training %>% 
  filter(is.na(subregion))

gapminder_training <- gapminder_training %>% 
  select(-country.y) %>% 
  rename("country" = country.x) %>% 
  relocate(c(region, subregion, iso_alpha, iso_num), .after = continent)
  
  
# model 4
mod4 <- lm(lifeExp ~ subregion + year + log2(gdpPercap), 
           data = gapminder_training)


summary(mod4)

mod4 %>% 
  pct_variation()

gapminder_training %>% 
  spread_predictions("mod1_pred" = mod1, 
                     "mod2_pred" = mod2,
                     "mod3_pred" = mod3,
                     "mod4_pred" = mod4) %>% 
  spread_residuals("mod1_resid" = mod1,
                   "mod2_resid" = mod2,
                   "mod3_resid" = mod3,
                   "mod4_resid" = mod4) %>% 
  View()


gridExtra::grid.arrange(
  
  gapminder_training %>% 
    data_grid(continent, year) %>% 
    mutate(pred = predict(mod1, .)) %>% 
    ggplot() +
    geom_line(aes(year, pred, color = continent)) + 
    labs(title = "mod1 - only \"continent\" as predictor"),
  
  gapminder_training %>% 
    data_grid(continent, year) %>% 
    mutate(pred = predict(mod2, .)) %>% 
    ggplot() +
    geom_line(aes(year, pred, color = continent)) + 
    labs(title = "mod2 - \"year\" as predictor added"),
  
  gapminder_training %>% 
    mutate(pred = predict(mod3, .)) %>% 
    ggplot(aes(year, pred, color = continent)) +
    geom_jitter(aes(group = continent)) + 
    geom_smooth(method = "lm",
                se = FALSE) + 
    labs(title = "mod3 - \"gdpPercap\" as predictor added"),

  gapminder_training %>% 
    mutate(pred = predict(mod4, .)) %>% 
    ggplot(aes(year, pred, color = subregion)) +
    geom_jitter(aes(group = subregion)) + 
    geom_smooth(method = "lm",
                se = FALSE) + 
    labs(title = "mod3 - \"subregion\" instead of \"continent\""),
  
  ncol = 2
  
)



gapminder_training %>% 
  spread_predictions("mod3_pred" = mod3, 
                     "mod4_pred" = mod4) %>% 
  spread_residuals("mod3_resid" = mod3,
                   "mod4_resid" = mod4) %>%
  mutate(resid_abs_diff = abs(mod4_resid) - abs(mod3_resid),
         performance = ifelse(resid_abs_diff < 0, "better", "worse"),
         rowid = fct_reorder(factor(rowid), resid_abs_diff)) %>% 
  ggplot(aes(rowid, resid_abs_diff, fill = performance)) + 
  geom_col() +
  scale_fill_manual(values = c("better" = "darkgreen",
                               "worse" = "red")) + 
  theme(axis.text.x = element_blank()) + 
  labs(title = "mod4 vs. mod3")



gapminder_training %>% 
  spread_predictions("mod3_pred" = mod3, 
                     "mod4_pred" = mod4) %>% 
  spread_residuals("mod3_resid" = mod3,
                   "mod4_resid" = mod4) %>%
  filter(!is.na(mod4_resid)) %>% 
  mutate(resid_abs_diff = abs(mod4_resid) - abs(mod3_resid),
         performance = ifelse(resid_abs_diff < 0, "better", "worse")) %>% 
  group_by(performance) %>% 
  summarise(sum_resi = sum(resid_abs_diff))


gapminder_training %>% 
  add_predictions(mod4) %>% 
  add_residuals(mod4) %>% 
  ggplot(aes(pred, lifeExp)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              se = FALSE)


# overall evaluation ------------------------------------------------------

gridExtra::grid.arrange(
  
  gapminder_training %>% 
    add_predictions(mod1) %>% 
    add_residuals(mod1) %>% 
    ggplot(aes(pred, lifeExp)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod1"),
  
  gapminder_training %>% 
    add_predictions(mod2) %>% 
    add_residuals(mod2) %>% 
    ggplot(aes(pred, lifeExp)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod2"),
  
  gapminder_training %>% 
    add_predictions(mod3) %>% 
    add_residuals(mod3) %>% 
    ggplot(aes(pred, lifeExp)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod3"),
  
  gapminder_training %>% 
    add_predictions(mod4) %>% 
    add_residuals(mod4) %>% 
    ggplot(aes(pred, lifeExp)) + 
    geom_point() + 
    geom_abline(color = "red",
                size = 1.5) + 
    labs(title = "mod4"),
  
  ncol = 2
)


gridExtra::grid.arrange(
  
  gapminder_training %>% 
    add_predictions(mod1) %>% 
    add_residuals(mod1) %>% 
    ggplot(aes(rowid, resid)) + 
    geom_point(alpha = 0.2) + 
    geom_hline(yintercept = 0,
               lty = 2,
               color = "blue") +
    expand_limits(y = c(30, -30)) +
    labs(title = "mod1"),
  
  gapminder_training %>% 
    add_predictions(mod2) %>% 
    add_residuals(mod2) %>% 
    ggplot(aes(rowid, resid)) + 
    geom_point(alpha = 0.2) + 
    geom_hline(yintercept = 0,
               lty = 2,
               color = "blue") +
    expand_limits(y = c(30, -30)) +
    labs(title = "mod2"),
  
  gapminder_training %>% 
    add_predictions(mod3) %>% 
    add_residuals(mod3) %>% 
    ggplot(aes(rowid, resid)) + 
    geom_point(alpha = 0.2) + 
    geom_hline(yintercept = 0,
               lty = 2,
               color = "blue") + 
    expand_limits(y = c(30, -30)) +
    labs(title = "mod3"),
  
  gapminder_training %>% 
    add_predictions(mod4) %>% 
    add_residuals(mod4) %>% 
    ggplot(aes(rowid, resid)) + 
    geom_point(alpha = 0.2) + 
    geom_hline(yintercept = 0,
               lty = 2,
               color = "blue") +
    expand_limits(y = c(30, -30)) + 
    labs(title = "mod4"),
  
  ncol = 2
)


# predict testing set -----------------------------------------------------

gapminder_testing <- gapminder_testing %>% 
  left_join(gapminder::country_codes,
            by = "country")
gapminder_testing <- gapminder_testing %>% 
  left_join(regions,
            by = c("iso_alpha" = "iso3"))

gapminder_testing <- gapminder_testing%>% 
  select(-country.y) %>% 
  rename("country" = country.x) %>% 
  relocate(c(region, subregion, iso_alpha, iso_num), .after = continent)


gapminder_testing <- gapminder_testing %>% 
  mutate(pred = predict(mod4, .),
         resid = lifeExp - pred)

View(gapminder_testing)
