
# packages and working directory ------------------------------------------
library(tidyverse)
theme_set(theme_minimal())



# load data ---------------------------------------------------------------
df_raw <- read_csv("spatial_data/UBD0028.csv",
                   locale = locale(encoding = "latin1"))

names(df_raw)

# take a look -------------------------------------------------------------
df_raw %>% 
  count(year,
        sort = TRUE) %>% 
  print(n = nrow(.))

# AI is missing...
df_raw %>% 
  count(station_canton) %>% 
  print(n = nrow(.))

df_raw %>% 
  ggplot(aes(year)) +
  geom_histogram(bins = length(min(df_raw$year):max(df_raw$year)))

df_raw %>% 
  count(year, station_canton) %>% 
  group_by(station_canton) %>% 
  add_count() %>% 
  select(nn, station_canton) %>% 
  distinct() %>% 
  print(n = nrow(.))
# complete data only for: "AG", "BE", "BL", "BS", "TG", "TI", "VD", "VS", "ZH"

df_raw %>% 
  count(area_type_de,
        sort = TRUE)

df_raw %>% 
  count(station_canton,
        sort = TRUE) %>% 
  print(n = nrow(.))

df_raw %>% 
  count(pollutant_name_de,
        pollutant_description_de,
        sort = TRUE) %>% 
  print(n = nrow(.))

df_raw %>% 
  count(aggregation_name_de,
        sort = TRUE)

df_raw %>% 
  count(unit_name_de,
        sort = TRUE)

df_raw %>% 
  count(value_remark)



# modify ------------------------------------------------------------------
df <- df_raw %>% 
  mutate(over_limit = value - limitvalue) %>%
  rename("long" = station_y,
         "lat" = station_x) %>% 
  relocate(year, .before = station_id) %>% 
  relocate(over_limit, .after = value)



# get map -----------------------------------------------------------------
tempdir <- tempdir()
map_CH <- raster::getData(name = "GADM", country = "CH", level = 1, path = tempdir)

map_CH

# need to transfrom coordinates (different CSR)
# see: https://spatialreference.org/ref/epsg/21781/
map_CH <- sp::spTransform(map_CH, CRS = "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs") 
map_CH

# create tidy tibble
map_CH_df <- map_CH %>% broom::tidy()



# get labels --------------------------------------------------------------
# get centroids first
centroids_CH <- rgeos::gCentroid(map_CH, byid = TRUE)

# create tibble
centroids_CH_df <- tibble(
  canton_id = row.names(centroids_CH),
  lat = centroids_CH@coords[,2],
  long = centroids_CH@coords[,1]
)

# get names
temp <- map_CH@data %>% 
  rownames_to_column(var = "canton_id") %>% 
  select(canton_id, NAME_1)

# join
labels <- inner_join(centroids_CH_df, temp, by = "canton_id") %>% 
  rename("canton" = NAME_1) %>% 
  mutate(canton_id = as.integer(canton_id))

# clean up
rm(temp)
gc()

# check
ggplot() +  
  geom_polygon(data = map_CH_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "grey50",
               color = "grey80") + 
  geom_text(data = labels,
            aes(x = long,
                y = lat,
                label = canton))



# take a look at O3 -------------------------------------------------------

ggplot() +  
  geom_polygon(data = map_CH_df,
               aes(x = long,
                   y = lat,
                   group = group),
               color = "white",
               fill = "grey80") + 
  geom_point(data = df %>% filter(pollutant_name_de == "O3",
                                  year %in% c(1990, 1995, 2000, 2005, 2010, 2015),
                                  aggregation_name_de == "maximales Stundenmittel"),
             aes(x = long,
                 y = lat,
                 size = over_limit),
             color = "red",
             alpha = 0.5) +
  geom_text(data = labels,
            aes(x = long,
                y = lat,
                label = canton)) +
  facet_wrap(~year) + 
  theme_void() + 
  labs(title = "Ozonverschmutzung in der Schweiz von 1990 - 2015",
       subtitle = "Werte in µg/m3\nGrenzwert (maximales Stundenmittel) bei 120 µg/m3)\n",
       size = "µg/m3 über dem Grenzwert")



# take a look at NO2 ------------------------------------------------------

ggplot() +  
  geom_polygon(data = map_CH_df,
               aes(x = long,
                   y = lat,
                   group = group),
               color = "white",
               fill = "grey80") + 
  geom_point(data = df %>% filter(pollutant_name_de == "NO2",
                                  year %in% c(1990, 1995, 2000, 2005, 2010, 2015),
                                  aggregation_name_de == "Jahresmittel",
                                  over_limit >= 0),
             aes(x = long,
                 y = lat,
                 size = over_limit),
             color = "red") +
  geom_text(data = labels,
            aes(x = long,
                y = lat,
                label = canton)) +
  facet_wrap(~year) + 
  theme_void() + 
  labs(title = "Stickstoffdioxidverschmutzung in der Schweiz von 1990 - 2015",
       subtitle = "Werte in µg/m3\nGrenzwert (Jahresmittel) bei 30 µg/m3)",
       size = "µg/m3 über dem Grenzwert")

