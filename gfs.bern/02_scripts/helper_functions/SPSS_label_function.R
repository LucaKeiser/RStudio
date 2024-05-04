
# Pakete ------------------------------------------------------------------
library(tidyverse)
library(haven)

# Funktion laden
source(here::here("gfs.bern",
                  "02_scripts",
                  "helper_functions",
                  "SPSS_summary_function_raw.R"))


# Daten laden -------------------------------------------------------------
df <- haven::read_sav(here::here("gfs.bern",
                                 "01_data",
                                 "ESS_subset.sav"))

# Excel mit allen notwendigen Informationen erstellen
# df %>% 
#   spss_summary(view = FALSE) %>% 
#   select(variable_name, variable_label, 
#          value_range, value_label) %>% 
#   writexl::write_xlsx(here::here("gfs.bern",
#                                  "01_data",
#                                  "df_labels.xlsx"))

df_labels <- readxl::read_xlsx(here::here("gfs.bern",
                                          "01_data",
                                          "df_labels.xlsx"))



# Labels-Funktion ---------------------------------------------------------

# Check
table(names(df) == df_labels$variable_name)

for(i in seq_along(names(df))) {
  
  
  ### Aktulle Variable
  current_variable <- names(df)[i]
  message(glue::glue("{i}. Variable: {current_variable}"))
  
  
  ### benötigte Informationen extrahieren
  
  # 1. Variable Label
  variable_label <- df_labels[[2]][[i]]
  
  if(is.na(variable_label)) {
    variable_label <- "KEIN LABEL GEFUNDEN"
  }
  
  # 2. Value Range extrahieren
  value_range <-  df_labels[[3]][[i]] %>% 
    str_squish() %>% 
    str_split("; ") %>% 
    unlist()
  
  if(!any(is.na(value_range)) & 
     all(str_detect(value_range, "[0-9]"))) {
    value_range <- parse_number(value_range)
  }
  
  # 3. Value Labels extrahieren
  value_label <- df_labels[[4]][[i]] %>% 
    str_split("; ") %>% 
    unlist() %>% 
    str_squish()
  
  
  ### 1. Option
  # Wenn die Anzahl Variable Labels mit der 
  # Anzahl Value Labels übereinstimmt:
  
  if(length(value_range) == length(value_label) & 
     !any(is.na(value_range)) & !any(is.na(value_label))) {
    
    value_labels <- setNames(value_range, value_label)
    
    df <- df %>% 
      mutate(
        {{ current_variable }} := 
          labelled_spss(
            df[, i][[1]],
            label = variable_label,
            labels = value_labels
          )
      )
  } 
  
  # Wenn die beiden Werte nicht übereinstimmen, 
  # wird nur das Variable Label vergeben.
  else{
    df <- df %>% 
      mutate(
        {{ current_variable }} := 
          labelled_spss(
            df[, i][[1]],
            label = variable_label
          )
      )
  }
}

