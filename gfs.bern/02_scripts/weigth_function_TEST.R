
# Pakete ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(survey)
library(anesrake)

# Funktion laden
source(here::here("gfs.bern",
                  "02_scripts",
                  "SPSS_summary_function_raw.R"))





# Daten laden -------------------------------------------------------------
df <- haven::read_sav(here::here("gfs.bern",
                                 "01_data",
                                 "ESS_subset.sav"))





# Beispiel reproduzieren --------------------------------------------------
data("anes04")

anes04 <- anes04 %>% 
  rowid_to_column() %>% 
  mutate(age_grouped = case_when(
    age < 25 ~ 1,
    between(age, 25, 50) ~ 2,
    age > 50 ~ 3)
  )

agetarg <- c(0.2, 0.5, 0.3)
names(agetarg) <- c(1, 2, 3)
marriedtarget <- c(0.4, 0.6)
names(marriedtarget) <- c(0, 1)

targets <- list(agetarg, marriedtarget)

names(targets) <- c("age_grouped", "married")

outsave <- anesrake(inputter = targets, 
                    dataframe = anes04, 
                    caseid = anes04$rowid,
                    verbose = TRUE)

weigths <- tibble(
  idno = outsave$caseid,
  weigth = outsave$weightvec
)




## Beispieldatensatz erstellen --------------------------------------------
df <- df %>% 
  select(name, essround, idno, agea,
         gndr, lrscale) %>% 
  filter(essround == max(essround)) %>% 
  mutate(
    age_group = 
      case_when(
        agea < 35 ~ 1,
        between(agea, 35, 45) ~ 2,
        between(agea, 46, 65) ~ 3,
        agea > 65 ~ 4
      )
  ) %>% 
  mutate(
    lrscaleg = 
      case_when(
        lrscale < 3 ~ 1,
        between(lrscale, 3, 6) ~ 2,
        lrscale > 6 ~ 3
      )
  ) %>% 
  mutate(age_group = 
           labelled_spss(age_group,
                         label =  "Alter gruppiert",
                         labels = c("-35" = 1,
                                    "36-45" = 2,
                                    "46-65" = 3,
                                    "65+" = 4)
           )
  ) %>% 
  mutate(lrscaleg = 
           labelled_spss(lrscaleg,
                         label =  "Links-Rechts-Skala gruppiert",
                         labels = c("links" = 1,
                                    "mitte" = 2,
                                    "rechts" = 3)
           )
  )





# Gewichtung erstellen ----------------------------------------------------

# Ist-Wert
df %>% 
  count(gndr) %>% 
  mutate(pct = n / sum(n))

df %>% 
  count(age_group) %>% 
  mutate(pct = n / sum(n))

df %>% 
  count(lrscaleg) %>% 
  mutate(pct = n / sum(n))


# Soll-Wert
gndr <- c(0.6, 0.4)
age_group <- c(0.3, 0.2, 0.35, 0.15)
lrscaleg <- c(0.25, 0.5, 0.25)

targets <- list(gndr,
                age_group,
                lrscaleg)

names(targets) <- c("gndr",
                    "age_group",
                    "lrscaleg")

# Check
map(targets, sum)



df_weigthed <- df %>% 
  filter(!is.na(gndr)) %>% 
  filter(!is.na(age_group)) %>% 
  filter(!is.na(lrscale)) %>% 
  zap_labels() %>% 
  zap_label()

my_weigths <- anesrake(
  inputter = targets,
  dataframe = df_weigthed,
  caseid = df_weigthed$idno
)














