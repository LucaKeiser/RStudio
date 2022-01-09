# load packages
library(tidyverse)
library(pdftools)

# get the pdf-file path
file <- "PDF Scraping/IFZ Sustainable Investments Studie 2021 ES-1_removed.pdf"      

# read in the text
pdf_document <- pdf_text(file) %>% 
  str_split("\n")



# clean up

# 1. remove the header (always the first 5 lines)

for (i in 1:length(pdf_document)) {
  # iterate through the list and delete the first 5 lines
  pdf_document[[i]] <- pdf_document[[i]][-1:-5]
}


# separte out each lines character => str_squish
pdf_document <- pdf_document %>% 
  str_squish()

# split lines by " , \" 
# use regex!
pdf_document <- pdf_document %>% 
  str_split(pattern = "\\,\\s\\\"")

# get more information on regex... They look damn complicated...
# remove the "c(\" at the start 
# expract everything that is preceeded by the "c(\"

for (i in 1:length(pdf_document)){
  pdf_document[[i]][1] <- pdf_document[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}


# and the \" at the end of the lines
# NOTE: we have to iterate through each row of each page
# So we need to use another for-statement!
# extract everthing prior to \"

for (i in 1:length(pdf_document)){
  for (j in 1:length(pdf_document)) {
    pdf_document[[i]][j] <- pdf_document[[i]][j] %>% 
      stringr::str_extract(".*(?=\")")
  }
}


# put everything that is a word in a pile
# put everything that is a number in a separate pile
# again we have to iterate thorugh all pages and all lines!

# testing fist
for (i in 1:length(pdf_document)) {
  for (j in 1:length(pdf_document)) {
    pdf_document[[i]][j] %>% 
      str_extract(".*[:alpha:]+") %>% 
      print()
  }
}



names_ex <- list()

for(i in 1:length(pdf_document)){
  words <- pdf_document[[i]] %>% 
    str_extract(".*[:alpha:]+")
  words_df <- as_tibble(words)
  names_ex[[i]] <- words_df
  NH_names <- dplyr::bind_rows(names_ex)
}


NH_names <- NH_names %>% 
  drop_na()




