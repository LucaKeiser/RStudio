
# load packages -----------------------------------------------------------

library(tidyverse)
library(pdftools)




# get data ----------------------------------------------------------------

df_raw <- pdf_text("Cyclotour_du_Lemon/2024/cyclotour_lemon_2024.pdf")





# prepare data ------------------------------------------------------------

### 1. split and unlist
df <- df_raw %>% 
  str_split("\n") %>% 
  unlist()

### 1. only include Lausanne - Lausanne | 176 km
# => 907 ZYWIOLEK Anna = last person on the PDF-list
df <- df[1:str_which(df, "907 ZYWIOLEK Anna")]


### 2. remove empty 
df <- df %>% 
  str_subset(".")

### 3. remove unnecessary stings
df <- df %>% 
  str_subset("Bib Name",
             negate = TRUE) %>% 
  str_subset("Front of Line Package | 176 km",
             negate = TRUE) %>% 
  str_subset("Lausanne - Lausanne | 176 km",
             negate = TRUE) %>% 
  str_subset("E-bike",
             negate = TRUE) %>% 
  str_subset("Road bike",
             negate = TRUE) %>% 
  str_subset("2024-06-18 18:18:14 Zeitmessung und Auswertung: race result Swiss Timing GmbH",
             negate = TRUE) %>% 
  str_subset("Start list",
             negate = TRUE)




# extract information -----------------------------------------------------

### 1. race time
race_time <- df %>% 
  str_extract("\\d:\\d\\d:\\d\\d")

# check
length(race_time) == length(df)
race_time[1]
race_time[length(race_time)]


### 2. IDs
race_ids <- df %>% 
  str_extract("(\\d)* ") %>% 
  parse_number()

# check
length(race_ids) == length(df)
race_ids[1]
race_ids[length(race_ids)]


### 3. data of birth
year_of_birth <- df %>% 
  str_extract(" \\d\\d\\d\\d ") %>% 
  parse_number()

# check
length(year_of_birth) == length(df)
year_of_birth[1]
year_of_birth[length(year_of_birth)]


### 4. nationality
# sting starts at position 41 and is 2 to 3 letters long
str_locate("2915 GALJOUR Joshua                     GE    1982 Geneva                       6:38:27", "GE")

nationality <- df %>% 
  str_sub(start = 41, 
          end = 43) %>% 
  str_squish()

# check
length(nationality) == length(df)
nationality[1]
nationality[length(nationality)]


### 5. names of participants
df_temp <- df %>% 
  str_remove_all("\\d|:")

names_participants <- df_temp %>% 
  str_trim(side = "left") %>% 
  str_extract("[\\s|\\s{2}|\\s{3}]?^(.*?)  ") %>% 
  str_squish()

# check
length(names_participants) == length(names_participants)
names_participants[1]
names_participants[length(names_participants)]


### 6. cycling team
df_temp <- df %>% 
  str_remove_all("\\d|:")

cycling_team <- df_temp %>% 
  str_trim(side = "left") %>% 
  str_remove_all(names_participants) %>% 
  str_remove_all(nationality) %>% 
  str_squish()

# check
length(cycling_team) == length(cycling_team)
cycling_team[1]
cycling_team[length(cycling_team)]





# create tibble -----------------------------------------------------------

df <- tibble(
  name = names_participants,
  country = nationality,
  team = cycling_team,
  year_of_birth = year_of_birth,
  time = race_time,
  start_number = race_ids
) %>% 
  mutate(time = hms(time),
         time_hour = as.numeric(time, 
                                unit = "hour")) %>% 
  mutate(age = 2024 - year_of_birth) %>% 
  rowid_to_column()





# save data ---------------------------------------------------------------

write_rds(x = df,
          file = "Cyclotour_du_Lemon/2024/2024_cyclotour.rds")

