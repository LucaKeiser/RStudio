
# SPSS-summary-function ---------------------------------------------------

### Packages needed
library(tidyverse)

### Function
spss_summary <- function(data,
                         sep = "; ",
                         view = TRUE,
                         cut_value_range = 9999,
                         cut_value_labels = 9999,
                         histogram_bins = 5) {
  
  
  # prepare data
  data_set_name <- substitute(data)
  data_filtered <- data %>% 
    janitor::remove_empty(which = "cols")
  
  
  ### 1. create summary-variables
  
  # 1.1. value range
  value_range <- sjlabelled::get_values(data_filtered) %>% 
    map(., ~str_c(., collapse = sep)) %>% 
    map(., ~str_sub(., end = cut_value_range)) %>%
    unlist()
  
  # 1.2. value labels
  value_label <- sjlabelled::get_labels(data_filtered) %>% 
    map(., ~str_c(., collapse = sep)) %>% 
    map(., ~str_sub(., end = cut_value_labels)) %>% 
    unlist() 
  
  # 1.3. min value
  min_value <- sjlabelled::as_numeric(data_filtered) %>% 
    map_dbl(., ~min(., na.rm = TRUE)) %>% 
    round(2)
  
  # 1.4. first quartile
  q25 <- sjlabelled::as_numeric(data_filtered) %>% 
    map_dbl(., ~quantile(., 0.25, 
                         na.rm = TRUE)) %>% 
    round(2)
  
  # 1.5. second quartile (median)
  q50_median <- sjlabelled::as_numeric(data_filtered) %>% 
    map_dbl(., ~median(., na.rm = TRUE)) %>% 
    round(2)
  
  # 1.6. mean value
  mean_value <- sjlabelled::as_numeric(data_filtered) %>% 
    map_dbl(., ~mean(., na.rm = TRUE)) %>% 
    round(2)
  
  # 1.7. third quartile 
  q75 <- sjlabelled::as_numeric(data_filtered) %>% 
    map_dbl(., ~quantile(., 0.75, 
                         na.rm = TRUE)) %>% 
    round(2)
  
  # 1.8. max value
  max_value <- sjlabelled::as_numeric(data_filtered) %>% 
    map_dbl(., ~max(., na.rm = TRUE)) %>% 
    round(2)
  
  # 1.9. histogram
  histogram <- sjlabelled::as_numeric(data_filtered)
  histogram <- map(histogram, ~table(cut(., histogram_bins)))
  histogram <- map(histogram, ~./max(.))
  
  # 1.10. variable_class
  variable_class <- map(data_filtered, class) %>% 
    map(., ~str_c(., collapse = sep)) %>% 
    unlist()
  
  
  ### 2. create tibble
  tibble <- tibble(
    orignial_order = 1:ncol(data_filtered),
    variable_name = names(data_filtered),
    variable_label = sjlabelled::get_label(data_filtered),
    value_range = value_range,
    value_label = value_label,
    number_NAs = map_dbl(data_filtered, ~sum(is.na(.))),
    percent_NAs = round((number_NAs / nrow(data_filtered)) * 100, 2),
    min_value = min_value,
    q25 = q25,
    q50_median = q50_median,
    q75 = q75,
    mean_value = mean_value,
    max_value = max_value,
    histogram = map(histogram, cli::spark_bar),
    variable_class = variable_class,
    variable_type = map_chr(data_filtered, typeof)
  )
  
  ### 3. show result
  
  # 3.1. view()
  if(view == TRUE){
    
    if(ncol(data_filtered) != ncol(data)) {
      
      removed_cols <- setdiff(names(data), names(data_filtered)) %>% 
        str_c(collapse = sep)
      
      message(glue("NOTE: The following colimns have been removed (no valid observations):{removed_cols}."))
      
    }
    
    View(tibble,
         title = as.character(data_set_name))
    
    # 3.2. print()
  } else{
    
    if(ncol(data_filtered) != ncol(data)) {
      
      removed_cols <- setdiff(names(data), names(data_filtered)) %>% 
        str_c(collapse = sep)
      
      message(glue("NOTE: The following colimns have been removed (no valid observations):{removed_cols}."))
      
    }
    
    return(tibble)
    
  }
  
}