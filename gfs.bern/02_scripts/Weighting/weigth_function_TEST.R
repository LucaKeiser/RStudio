
# Pakete ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(survey)
library(anesrake)

# Funktion laden
source(here::here("gfs.bern",
                  "02_scripts",
                  "helper_functions",
                  "SPSS_summary_function_raw.R"))





# Beispiel reproduzieren --------------------------------------------------

## Daten laden ------------------------------------------------------------
data("anes04")

# aufbereiten
anes04 <- anes04 %>% 
  rowid_to_column() %>% 
  mutate(age_grouped = case_when(
    age < 25 ~ 1,
    between(age, 25, 50) ~ 2,
    age > 50 ~ 3)
  )


## Soll-Werte definieren (aka Targets) ------------------------------------
agetarg <- c(0.2, 0.5, 0.3)
names(agetarg) <- c(1, 2, 3)
marriedtarget <- c(0.4, 0.6)
names(marriedtarget) <- c(0, 1)

targets <- list(agetarg, marriedtarget)
names(targets) <- c("age_grouped", "married")


## Gewichtung berechnen ---------------------------------------------------
weigths <- anesrake(inputter = targets, 
                    dataframe = anes04, 
                    caseid = anes04$rowid,
                    verbose = TRUE)

# mit Daten mergen
weigths <- tibble(
  rowid = weigths$caseid,
  gewdef = weigths$weightvec
)

anes04 <- anes04 %>% 
  arrange(rowid) %>% 
  bind_cols(weigths %>% 
              arrange(rowid) %>% 
              select(gewdef)) %>% 
  as_tibble()


## Check ------------------------------------------------------------------
# age_grouped
anes04 %>% 
  count(age_grouped,
        wt = gewdef) %>% 
  mutate(pct = n / sum(n))

# married
anes04 %>% 
  count(married,
        wt = gewdef) %>% 
  mutate(pct = n / sum(n))





# Eigenes Beispiel --------------------------------------------------------

## Daten laden ------------------------------------------------------------
df <- haven::read_sav(here::here("gfs.bern",
                                 "01_data",
                                 "ESS_subset.sav"))

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
  )


## Soll-Werte definieren (aka Targets) ------------------------------------
gndr <- c(0.6, 0.4)
names(gndr) <- c(1, 2)

age_group <- c(0.3, 0.2, 0.35, 0.15)
names(age_group) <- c(1, 2, 3, 4)

lrscaleg <- c(0.25, 0.5, 0.25)
names(lrscaleg) <- c(1, 2, 3)

targets <- list(gndr, age_group, lrscaleg)
names(targets) <- c("gndr",
                    "age_group",
                    "lrscaleg")


## Gewichtung berechnen ---------------------------------------------------
weigths <- anesrake(inputter = targets,  
                    dataframe = df,
                    caseid = df$idno,
                    verbose = TRUE)

# mit Daten mergen
weigths <- tibble(
  idno = weigths$caseid,
  gewdef = weigths$weightvec
)

df_gewdef <- df %>% 
  left_join(weigths,
            by = "idno") %>% 
  # NAs werden auf 1 gesetzt
  mutate(gewdef = ifelse(is.na(gewdef),
                         1, 
                         gewdef))



## Check ------------------------------------------------------------------
df_gewdef %>% 
  count(gndr,
        wt = gewdef) %>% 
  mutate(pct_data = n / sum(n),
         pct_target = targets$gndr,
         difference = pct_target - pct_data)

df_gewdef %>% 
  filter(!is.na(age_group)) %>% 
  count(age_group,
        wt = gewdef) %>% 
  mutate(pct_data = n / sum(n),
         pct_target = targets$age_group,
         difference = pct_target - pct_data)

df_gewdef %>% 
  filter(!is.na(lrscaleg)) %>%
  count(lrscaleg,
        wt = gewdef) %>%
  mutate(pct_data = n / sum(n),
         pct_target = targets$lrscale,
         difference = pct_target - pct_data)

