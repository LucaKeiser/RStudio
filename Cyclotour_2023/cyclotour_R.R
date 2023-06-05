
# load packages -----------------------------------------------------------

library(tidyverse)
library(RSelenium)
library(rvest)
library(polite)
library(glue)

theme_set(theme_minimal())




# set up ------------------------------------------------------------------

remote_server <- rsDriver(port = 4567L,
                          browser = "firefox",
                          chromever = NULL)
# separate client and server
browser <- remote_server$client
server <- remote_server$server

# get data ----------------------------------------------------------------

target_url <- "https://www.datasport.com/live/ranking/?lang=de&racenr=25466#1_EE4EA7"
browser$navigate(target_url)

page <- browser$getPageSource()[[1]] %>% 
  read_html()

table_temp <- page %>% 
  html_element(css = ".MainTable") %>% 
  html_table()




# convert tot tibble ------------------------------------------------------

df <- tibble(
  name = table_temp$Name,
  country = table_temp$Nat.,
  city = table_temp$Ort,
  team = table_temp$Verein,
  year_of_birth = table_temp$Jg.,
  time = table_temp$Zeit,
  start_number = table_temp$Startnr.
) %>% 
  rowid_to_column()


# data wrangling ----------------------------------------------------------

# set bike types
df <- df %>% 
  mutate(bike_type = case_when(
    rowid <= 9 ~ "electric",
    rowid > 10 & rowid <= 2793 ~ "normal",
    TRUE ~ "tandem"
  ))

# delete unneccessary rows/columns and set column types
df <- df %>% 
  slice(-c(1, 10, 11, 2794, 2795)) %>% 
  mutate(year_of_birth = as.integer(year_of_birth),
         time = hms(time),
         time_hour = as.numeric(time, 
                                unit = "hour"),
         start_number = as.integer(start_number)) %>% 
  select(- c(rowid, time)) %>% 
  relocate(start_number, name, year_of_birth, 
           country, city, team,
           "time" = time_hour, bike_type)
  
# check
glimpse(df)



# plots -------------------------------------------------------------------

df %>% 
  ggplot(aes(time)) +
  geom_histogram(color = "white",
                 fill = "seagreen",
                 alpha = 0.75) +
  
  # We
  geom_vline(xintercept = 7.038889,
             lty = 5,
             linewidth = 2,
             color = "red") +
  geom_label(x = 7.038889, 
             y = 185, 
             label = "Arvid\nLuca\nAndy") +
  
  # Top 10%
  geom_vline(xintercept = quantile(df$time, 0.10, 
                                   na.rm = TRUE),
             linewidth = 1.25,
             alpha = 0.5) +
  annotate(geom = "label",
           x = quantile(df$time, 0.10, 
                        na.rm = TRUE), 
           y = 150, 
           label = "Top 10%") +
  
  # Top 25%
  geom_vline(xintercept = quantile(df$time, 0.25, 
                                   na.rm = TRUE),
             linewidth = 1.25,
             alpha = 0.5) +
  annotate(geom = "label",
             x = quantile(df$time, 0.25, 
                          na.rm = TRUE), 
             y = 125, 
             label = "Top 25%") +
  
  # Top 50%
  geom_vline(xintercept = quantile(df$time, 0.5, 
                                   na.rm = TRUE),
             linewidth = 1.25,
             alpha = 0.5) +
  annotate(geom = "label",
             x = quantile(df$time, 0.5, 
                          na.rm = TRUE), 
             y = 150, 
             label = "Top 50%") +
  
  # Top 75%
  geom_vline(xintercept = quantile(df$time, 0.75, 
                                   na.rm = TRUE),
             linewidth = 1.25,
             alpha = 0.5) +
  annotate(geom = "label",
           x = quantile(df$time, 0.75, 
                          na.rm = TRUE), 
             y = 125, 
             label = "Top 75%") +
  
  # Top 90%
  geom_vline(xintercept = quantile(df$time, 0.90, 
                                   na.rm = TRUE),
             linewidth = 1.25,
             alpha = 0.5) +
  annotate(geom = "label",
           x = quantile(df$time, 0.90, 
                        na.rm = TRUE), 
           y = 150, 
           label = "Top 90%") +
  
  scale_x_continuous(breaks = seq(4, 10, 0.25)) +
  labs(title = "\nHow did WE do overall?",
       subtitle = glue("{nrow(df)} Participants\n"),
       x = "\nTime (hours)",
       y = "Count\n")

