
# Pakete ------------------------------------------------------------------
library(tidyverse)
library(haven)


# Daten laden -------------------------------------------------------------
df <- tibble(
  var_1 = c(0, 1, 1, 1, 0, 0, 99999999, 99999998),
  var_2 = c(1, 1, 2, 4, 5, 3, 1, 99999999),
  var_3 = c(99999999, 5, 4, 4, 4, 3, 5, 1)
)

df_labels <- readxl::read_xlsx(here::here("gfs.bern",
                                          "01_data",
                                          "df_labels.xlsx"),
                               col_names = c("variable", "variable_label", 
                                             "value_range", "value_label"))


# Labels-Funktion ---------------------------------------------------------

# Check
names(df) == df_labels$variable

for(i in seq_along(names(df))) {
  
  current_variable <- names(df)[i]
  
  message(glue::glue("{current_variable}"))
  
  # Value Range extrahieren
  value_range <-  df_labels[[3]][[i]] %>% 
    str_remove_all(";|9999998|99999999") %>% 
    str_squish() %>% 
    str_split(" ") %>% 
    unlist() %>% 
    as.numeric()
  
  # Value Labels extrahieren
  value_labels <- df_labels[[4]][[i]] %>% 
    str_remove_all(";") %>% 
    str_split(" ") %>% 
    unlist() %>% 
    str_squish()
  
  variable_label <- df_labels[[2]][[i]]
  value_labels <- setNames(value_range, value_labels)
  
  # Datensatz belabeln
  df <- df %>% 
    mutate(
      {{ current_variable }} := 
        labelled_spss(
          df[, i][[1]],
          label = variable_label,
          labels = value_labels,
          na_values = c(9999998, 9999999)
        )
    )
}

