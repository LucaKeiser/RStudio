# load packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(RSelenium)
library(polite)



# check if it is okay to scrape first -------------------------------------

target_url <- "https://tel.search.ch/"
bow(url = target_url)


# start a server ----------------------------------------------------------

remote_server <- rsDriver(browser="firefox")

# separate client and server
browser <- remote_server$client
server <- remote_server$server



# scrape data -------------------------------------------------------------

# 1) navigate to the target url
browser$navigate(url = target_url)


# 2) find search bars
search_bar <- browser$findElement(using = "css selector",
                                  value = ".tel-input+ .tel-input .tel-feedback")

# 3) type in text
search_bar$sendKeysToElement(list(value = "Aargau",
                                  key = "enter"))


# 4) scrape content
body <- browser$findElement(using = "css selector",
                            value = "body")

for (i in 1:10){
  
  page <- bow(url = browser$getCurrentUrl()[[1]]) %>% 
    scrape()
  
  df <- tibble(
    "Name" = page %>% 
      html_elements(css = ".tel-resultentry-clickable h1 a") %>% 
      html_text(),
    
    "Adresse" = page %>% 
      html_elements(css = ".tel-address") %>% 
      html_text(),
    
    "Telefon" = page %>% 
      html_elements(css = ".sl-icon-call") %>% 
      html_text()
  )
  
  body$sendKeysToElement(list(key = "page_down"))
  
  Sys.sleep(5)
}
