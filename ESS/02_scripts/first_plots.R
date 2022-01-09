# load packages
library(tidyverse)

# load object
ESS <- read_rds("ESS/05_objects/ESS_clean.rds")

# create ESS-small for plotting
ESS_small <- ESS %>% 
  select(cntry, year, ppltrst, trstplc, trstlgl) %>% 
  mutate(ppltrst_rec = ifelse(ppltrst > 10, NA, ppltrst),
         trstplc_rec = ifelse(trstplc > 10, NA, trstplc),
         trstlgl_rec = ifelse(trstlgl > 10, NA, trstlgl))


plot_1 <- ESS_small %>% 
  group_by(cntry, year) %>% 
  mutate(ppltrst_rec_mean = mean(ppltrst_rec, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cntry = fct_reorder(cntry, ppltrst_rec_mean, .desc = TRUE)) %>% 
  ggplot(aes(x = year, y = ppltrst_rec_mean, color = cntry, group = cntry)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2002, 2018, 4)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_light() +
  theme(legend.position = "none") + 
  labs(
    title = "Trust in other people",
    x = "",
    y = ""
  )


plot_2 <- ESS_small %>% 
  group_by(cntry, year) %>% 
  mutate(trstplc_rec_mean = mean(trstplc_rec, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cntry = fct_reorder(cntry, trstplc_rec_mean, .desc = TRUE)) %>% 
  ggplot(aes(x = year, y = trstplc_rec_mean, color = cntry, group = cntry)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2002, 2018, 4)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_light() +
  theme(legend.position = "none")  +
  labs(
    title = "Trust in the Police",
    x = "Year",
    y = ""
  )


plot_3 <- ESS_small %>% 
  group_by(cntry, year) %>% 
  mutate(trstlgl_rec_mean = mean(trstlgl_rec, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cntry = fct_reorder(cntry, trstlgl_rec_mean, .desc = TRUE)) %>% 
  ggplot(aes(x = year, y = trstlgl_rec_mean, color = cntry, group = cntry)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2002, 2018, 4)) +
  scale_y_continuous(limits = c(0, 10)) +
  theme_light() +
  labs(
    title = "Trust in the Legal System",
    x = "",
    y = "",
    color = ""
  )

# create one plot with patchwork 
library(patchwork)
plot_1 + plot_2 + plot_3
