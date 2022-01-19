# Source
# => https://rstats-tips.net/2021/02/07/converting-lists-of-lists-of-lists-to-data-frames-or-tibbles/

# load packages
library(tidyverse)



# create list_entry-funtion -----------------------------------------------

set.seed(21)

list_entry <- function(some_intput) {
  list(
    sample(x = letters, size = 1),
    sample(x = 1:100, size = 1),
    list(paste0("Sublist ", round(runif(n = 1, min = 0, max = 100))), 
         paste0("Sublist ", round(runif(n = 1, min = 0, max = 100)))
    ),
    runif(n = 1, min = 0, max = 100)
  )
}




# create a list and convert -----------------------------------------------

n_small <- 5
list_of_list <-map(as.list(1:n_small), list_entry)


# convert into a data frame or tibble
# use purrr!
list_of_list %>% 
  purrr::map_dfr(as_tibble, .name_repair = "universal")
# good, but not what we wanted...
# every list should be a seperate row


# define column names
column_names <- letters[1:5]

# define function
convert_function <- function(list_input) {
  list_input <- as.data.frame(list_input)
  colnames(list_input) <- column_names
  list_input
}


tictoc::tic()

tibble_1 <- list_of_list %>%
  map(convert_function) %>%
  bind_rows() %>% 
  as_tibble()

tictoc::toc()



# performance test --------------------------------------------------------

n_large <- 100000
large_list_of_list <-map(as.list(1:n_large), list_entry)


# define column names
column_names <- letters[1:5]


# define function

convert_function_large <- function(large_list) {
  large_list <-  purrr::flatten(large_list)
  large_list <-  set_names(large_list, column_names)
  large_list
}


# measure time

tictoc::tic()

tibble_2 <- large_list_of_list %>% map_dfr(convert_function_large) %>% 
  as_tibble()

tictoc::toc()

