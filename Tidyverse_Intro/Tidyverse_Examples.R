# Intro - Tidyverse
# A brief overview



# package
library(tidyverse)



# readr -------------------------------------------------------------------

## read_csv ----------------------------------------------------------------
df_kinos <- read_csv("Tidyverse_Intro/Data/kinostandorte_1907-2018.csv")

# use spec()!
df_kinos %>% 
  spec()

# read in again
df_kinos <- read_csv("Tidyverse_Intro/Data/kinostandorte_1907-2018.csv",
               col_types = cols(
                 X_LV95 = col_double(),
                 Y_LV95 = col_double(),
                 Adresse = col_character(),
                 Kinonamen_history = col_character(),
                 Kinonamen = col_character(),
                 VonISO = col_date(format = ""),
                 BisISO = col_date(format = "")
                 ),
               col_names = c("x", "y", "adresse", "kinonamen_alt", "kinonamen", "von", "bis"),
               skip = 1 # need to skip one row, in order to get rid of the header....
               )

# check
df_kinos



# tidyr -------------------------------------------------------------------

## pivot_longer ------------------------------------------------------------
billboard %>% 
  pivot_longer(cols = wk1:wk76, 
               names_to = "week", 
               values_to = "rank")

billboard %>% 
  pivot_longer(cols = wk1:wk76, 
               names_to = "week", 
               values_to = "rank") %>% 
  drop_na() %>% 
  pivot_wider(names_from = week,
              values_from = rank,
              values_fill = -999)

# replace NAs with a numer (drop_na first)
billboard %>% 
  pivot_longer(cols = wk1:wk76, 
               names_to = "week", 
               values_to = "rank") %>% 
  drop_na() %>%
  pivot_wider(names_from = week,
              values_from = rank,
              values_fill = -999)


# create 0-1-dataframe
billboard %>% 
  select(artist, track) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = artist, values_from = n, values_fill = 0)



## separete_rows -----------------------------------------------------------
billboard %>% 
  separate_rows(artist, sep = ", ")



# dplyr -------------------------------------------------------------------

## mutate -----------------------------------------------------------------
iris %>% 
  as_tibble() %>% 
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         lager_than_five = if_else(condition = Sepal.Length > 5,
                                   true = "Greater than five",
                                   false = "Not greater than five"))




## count ------------------------------------------------------------------
iris %>% 
  count(Sepal.Length, sort = TRUE)

iris %>% 
  add_count(Sepal.Length)


## combine -----------------------------------------------------------------
iris %>% 
  as_tibble() %>% 
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         lager_than_five = if_else(condition = Sepal.Length > 5,
                                   true = "Greater than five",
                                   false = "Not greater than five")) %>% 
  count(Species, 
        lager_than_five) %>% 
  pivot_wider(names_from = lager_than_five, 
              values_from = n)



iris %>% 
  as_tibble() %>% 
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         lager_than_five = if_else(condition = Sepal.Length > 5,
                                   true = "Greater than five",
                                   false = "Not greater than five")) %>% 
  count(Species, 
        lager_than_five, sort = TRUE, name = "count")





## filter ------------------------------------------------------------------
iris %>% 
  filter(Species == "setosa")

iris %>% 
  filter(Species != "setosa")


iris %>% 
  filter(Species %in% c("setosa", "virginica"))


iris %>% 
  filter(!Species %in% c("setosa", "virginica"))


iris %>% 
  filter(Species == "setosa" | Sepal.Length > 5)


iris %>% 
  filter((Species == "setosa" & Sepal.Length < 5) | 
           (Species == "versicolor" & Petal.Length > 1))

iris %>% 
  filter(cume_dist(Sepal.Length) >= 0.95)

iris %>% 
  filter(dense_rank(desc(Sepal.Length)) < 5)


## select ------------------------------------------------------------------
iris %>% 
  select(Petal.Length)

iris %>% 
  select(Sepal.Length:Petal.Length)

iris %>% 
  select(-Sepal.Length)



## group_by ----------------------------------------------------------------
iris %>% 
  group_by(Species) %>% 
  summarise(avg_sepal_length = mean(Sepal.Length))

# if you want to keep the other variables => mutate!
iris %>% 
  group_by(Species) %>% 
  mutate(avg_sepal_length = mean(Sepal.Length))


iris %>% 
  group_by(Species) %>% 
  mutate(avg_sepal_length_group = mean(Sepal.Length)) %>% 
  ungroup() %>% 
  mutate(avg_sepal_length = mean(Sepal.Length))



iris %>% 
  group_by(Species) %>% 
  summarise(avg_length = mean(Sepal.Length),
            obs = n())




## case_when ---------------------------------------------------------------
iris %>% 
  mutate(test = case_when(
    Sepal.Length > 5 & Sepal.Width > 3 ~ "Large",
    Sepal.Length < 5 & Sepal.Width < 3 ~ "Small",
    TRUE ~ "Medium" # all that is left...
  ))


# compare to if_else
iris %>% 
  mutate(if_else(Sepal.Length > 5 & Sepal.Width > 3, "Large",
                 if_else(Sepal.Length < 5 & Sepal.Width < 3, 
                         "Small", 
                         "Medium")
                 )
         )



## window-functions --------------------------------------------------------
economics %>% 
  select(date, unemploy) %>% 
  mutate(prev_unemploy = lag(unemploy, 
                             n = 1, 
                             order_by = date))


economics %>% 
  select(date, unemploy) %>% 
  mutate(prev_unemploy = lag(unemploy, 
                             n = 1, 
                             order_by = date,
                             default = min(unemploy))) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = unemploy)) +
  geom_point(aes(y = prev_unemploy))




## ranking -----------------------------------------------------------------
iris %>% 
  as_tibble() %>% 
  arrange(desc(Sepal.Length)) %>% 
  mutate(s_length_rank = row_number())


# group it into buckets
iris %>% 
  as_tibble() %>% 
  arrange(desc(Sepal.Length)) %>% 
  mutate(s_length_rank = ntile(x = Sepal.Length, n =  5))
  

iris %>% 
  as_tibble() %>% 
  arrange(desc(Sepal.Length)) %>% 
  mutate(s_length_rank = dense_rank(Sepal.Length)) %>% 
  View()



## slice -------------------------------------------------------------------
iris %>% 
  slice(1:6)

iris %>% 
  slice_max(Sepal.Length)

iris %>% 
  slice_min(Sepal.Length)

iris %>% 
  arrange(desc(Sepal.Length)) %>% 
  slice(1:15)




## joining/merging ---------------------------------------------------------
band_members
band_instruments2

band_members %>% 
  left_join(band_instruments2, by = c("name" = "artist")) # the two are the same but have a different name


df <- tibble(
  x = c(1, 2, 3),
  y = c(4, 5, 6),
  z = c(7, 8, 9)
)

df_2 <- tibble(
  x = c(1, 2, 3),
  b = c(4, 5, 6),
  c = c(7, 8, 9)
)

# inner_join keeps all the matches!
df %>% 
  inner_join(df_2, by = "x")


# anti_join removes any of the machtes!
df_2 %>% 
  anti_join(df %>% 
              filter(z < 5), by = "x")


# setdiff => remove duplicates
iris_2 <- iris %>% 
  slice(1:10)

setdiff(iris, iris_2)
setdiff(iris, iris)


# interscet => takes duplicates
intersect(iris, iris_2)
intersect(iris, iris)



# stringr -----------------------------------------------------------------


## str_trim and str_squish -------------------------------------------------
df_kinos %>% 
  mutate(adresse = str_squish(adresse))


# example
# str_trim removes spaces at the beginning/end
"   aksdjf     asö   ldefj   aklsdjfjk  " %>% 
  str_trim()

# str_squish also removes repeated spaces within a word
"   aksdjf     asö   ldefj   aklsdjfjk  " %>% 
  str_squish()




## str_to_... --------------------------------------------------------------
# df is already str_to_title
df_kinos %>% 
  mutate(kinonamen = str_to_lower(kinonamen))

df_kinos %>% 
  mutate(kinonamen_alt = str_to_upper(kinonamen_alt))



# na_if (dplyr) ------------------------------------------------------------
# convert spaces into NAs
df_kinos %>% 
  mutate(adresse = na_if(x = adresse,
                         y = " "))

# example
"   " %>% 
  str_squish() %>% 
  na_if("")



## str_detect --------------------------------------------------------------

df_kinos %>% 
  filter(str_detect(adresse, "Albis"))

# compare to filter alone
df_kinos %>% 
  filter(adresse == "Albis")

df_kinos %>% 
  filter(adresse %in% c("Albisriederplatz", "Albisstrasse 44"))



# forcats -----------------------------------------------------------------

# read in new data
df_fight_data <- read_csv("https://raw.githubusercontent.com/andrew-couch/Tidy-Tuesday/master/Season%202/Data/fight_data.csv")

df_fight_data %>% 
  spec()

# bit of cleaning
df_fight_clean <- df_fight_data %>% 
  select(weight_class, kd, strike_landed, sig_strike_landed, td_landed) %>% 
  separate(col = weight_class, 
          into = c("gender", "weightclass"),
          sep = "_")
         
# graph
df_fight_clean %>% 
  group_by(gender, weightclass) %>% 
  summarise(median_strike_landed = median(strike_landed, na.rm = TRUE)) %>% 
  ungroup() %>%
  ggplot(aes(x = weightclass, y = median_strike_landed, fill = gender)) +
  geom_col() + 
  coord_flip()

# clean data a bit more
df_fight_clean <- df_fight_clean %>% 
  group_by(gender, weightclass) %>% 
  summarise(median_strike_landed = median(strike_landed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!str_detect(string = weightclass,
                     pattern = "Catch")) %>% 
  mutate(median_strike_landed = if_else(condition = gender == "Men",
                                        true = median_strike_landed,
                                        false = -median_strike_landed))

df_fight_clean %>%
  ggplot(aes(x = weightclass, y = median_strike_landed, fill = gender)) +
  geom_col() + 
  coord_flip()
    
    

## fct_reorder -------------------------------------------------------------
df_fight_clean %>% 
  mutate(weightclass = fct_reorder(weightclass, median_strike_landed,
                                   .desc = FALSE,
                                   .fun = median)) %>% 
  ggplot(aes(x = weightclass, y = median_strike_landed, fill = gender)) +
  geom_col() + 
  coord_flip()

# define
df_fight_clean <- df_fight_clean %>% 
  mutate(weightclass_order = case_when(
    weightclass == "Strawweight" ~ 1,
    weightclass == "Flyweight" ~ 2,
    weightclass == "Bantamweight" ~ 3,
    weightclass == "Featherweight" ~ 4,
    weightclass == "Lightweight" ~ 5,
    weightclass == "Welterweight" ~ 6,
    weightclass == "Middleweight" ~ 7,
    TRUE ~ 8
  ))

# check
df_fight_clean %>% 
  count(weightclass, weightclass_order) %>% 
  arrange(weightclass_order)

df_fight_clean %>% 
  mutate(weightclass = fct_reorder(weightclass, weightclass_order,
                                   .desc = TRUE)) %>% 
  ggplot(aes(x = weightclass, y = median_strike_landed, fill = gender)) +
  geom_col() + 
  coord_flip()
  


## fct_lump ----------------------------------------------------------------
# take a look
df_fight_data %>% 
  count(method, sort = TRUE)

# change
df_fight_data %>% 
  mutate(method = fct_lump(method, n = 4)) %>% 
  count(method, sort = TRUE)

df_fight_data %>% 
  mutate(method = fct_lump_lowfreq(method)) %>% 
  count(method, sort = TRUE)

# plot
df_fight_data %>% 
  mutate(method = fct_lump_min(method, min = 161)) %>% 
  count(method, sort = TRUE) %>%
  ggplot(aes(x = method, y = n)) +
  geom_col()

# change position of "other" => nice!
df_fight_data %>% 
  mutate(method = fct_lump_min(method, min = 161),
         method = fct_relevel(method, "Other", "Decision")) %>% 
  count(method, sort = TRUE) %>% 
  ggplot(aes(x = method, y = n)) + 
  geom_col()


    
