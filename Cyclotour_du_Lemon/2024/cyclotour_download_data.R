
# load packages -----------------------------------------------------------

library(tidyverse)
library(RSelenium)
library(rvest)
library(polite)



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

# save data
write_rds(x = df,
          file = "Cyclotour_2023/cyclotour.rds")



# close connection --------------------------------------------------------

browser$close()
server$stop()

