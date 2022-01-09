# load packages
library(tidyverse)
library(lubridate)


# load data
ESS <- read_csv("ESS/01_data/ESS_data.csv") 


# a bit of cleaning
ESS_clean <- ESS %>% 
  mutate(year = case_when(
    essround == 1 ~ 2002,
    essround == 2 ~ 2004,
    essround == 3 ~ 2006,
    essround == 4 ~ 2008,
    essround == 5 ~ 2010,
    essround == 6 ~ 2012,
    essround == 7 ~ 2014,
    essround == 8 ~ 2016,
    essround == 9 ~ 2018
  ),
  cntry = case_when(cntry == "BE" ~ "Belgium",
                    cntry == "NO" ~ "Norway",
                    cntry == "FI" ~ "Finland",
                    cntry == "CH" ~ "Switzerland",
                    cntry == "DE" ~ "Germany",
                    cntry == "SE" ~ "Sweden",
                    cntry == "IE" ~ "Ireland",
                    cntry == "NL" ~ "Netherlands",
                    cntry == "ES" ~ "Spain",
                    cntry == "GB" ~ "UK",
                    cntry == "FR" ~ "France",
                    cntry == "PT" ~ "Portugal",
                    cntry == "HU" ~ "Hungary",
                    cntry == "PL" ~ "Poland",
                    cntry == "SI" ~ "Slovenia"))


# save as RDS-object
write_rds(ESS_clean, "ESS/05_objects/ESS_clean.rds")


