
# Setup -------------------------------------------------------------------

# Packages
library(tidyverse)
library(tidytext)
library(estimatr)
library(modelr)

theme_set(theme_minimal())
options(scipen = 999)

# Data
tuesdata <- tidytuesdayR::tt_load('2024-10-29')
df <- tuesdata$monster_movies


df_long <- df %>% 
  unnest_tokens(output = "genres_long_raw",
                input = genres,
                token = "regex",
                pattern =  ",",
                drop = FALSE) %>% 
  mutate(genres_long = fct_lump(genres_long_raw, 8),
         genres_long = if_else(is.na(genres_long), "Other", genres_long))


# EDA ---------------------------------------------------------------------

df %>% 
  glimpse()


### y-variable (average rating)
summary(df$average_rating)
df %>% 
  ggplot(aes(average_rating)) + 
  geom_histogram(color = "white",
                 bins = 20) +
  stat_bin(geom = "label",
           bins = 20,
           aes(label = after_stat(count)),
           vjust = 1) +
  labs(x = "Avg. Rating",
       y = "")


### potential x-variables
df %>% 
  filter(year >= 1950) %>% 
  ggplot(aes(year, 
             average_rating)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df %>% 
  filter(runtime_minutes <= 180) %>% 
  ggplot(aes(runtime_minutes,
             average_rating)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df %>% 
  filter(num_votes <= 1000) %>% 
  ggplot(aes(num_votes,
             average_rating)) + 
  geom_point() + 
  geom_smooth(method = "lm")

df %>% 
  ggplot(aes(title_type,
             average_rating,
             fill = title_type)) + 
  geom_boxplot()


df_long %>% 
  mutate(genres_long = fct_reorder(genres_long, average_rating)) %>% 
  ggplot(aes(genres_long,
             average_rating,
             fill = genres_long)) + 
  geom_boxplot()
  




# Linear Model ------------------------------------------------------------

m1 <- lm(average_rating ~ year,
         data = df)
summary(m1)

m2 <- lm(average_rating ~ year + runtime_minutes, 
         data = df)
summary(m2)

m3 <- lm(average_rating ~ year + runtime_minutes + num_votes,
         data = df)
summary(m3)

m4 <- lm(average_rating ~ year + runtime_minutes + num_votes + title_type,
         data = df)
summary(m4)

m5 <- lm_robust(average_rating ~ year + runtime_minutes + num_votes + title_type + genres_long,
                data = df_long,
                clusters = tconst,
                se_type = "CR2")
summary(m5)

sjPlot::tab_model(m1, m2, m3, m4, m5)

