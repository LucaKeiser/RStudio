# source: https://resulumit.com/teaching/scrp_workshop.html#130

library(tidyverse)
library(rvest)
library(RSelenium)
library(polite)



# start a server ----------------------------------------------------------

rD <- rsDriver(browser="firefox")

# separate the client and server as different objects
browser <- rD$client
server <- rD$server



# interacting with the browser --------------------------------------------

browser$navigate(url = "https://luzpar.netlify.app")

browser$refresh()

browser$getCurrentUrl()

browser$getTitle()


# Note that the methods return a list
# the XML is in the first item => [[1]]
# we can convert the XML to hmtl with read_html

browser$getPageSource()[[1]] %>% 
  # convert XML into html => we can work with rvest!
  read_html() %>% 
  html_elements("#title a") %>% 
  html_attr("href")




# find objects on a page --------------------------------------------------

# note that we can only scrape what we see on our screen
# so maximazing the window can be useful!
browser$maxWindowSize()

# find the States-menu and store it as an object to interact with it later on!

menu_states <- browser$findElement(using = "link text",
                                   value = "States")

# OR with css selectors

menu_states_2 <- browser$findElement(using = "css selector",
                                     value = ".nav-item:nth-child(2) span")

menu_states$highlightElement()
menu_states_2$highlightElement()


search_icon <- browser$findElement(using = "css selector",
                                   value = ".fa-search")



# click on the element ----------------------------------------------------

menu_states$clickElement()

# and go back again

browser$goBack()


search_icon$clickElement()

# and refresh the page again

browser$refresh()




# combine the steps -------------------------------------------------------

# get an overview of the possible interactions
as_tibble(selKeys) %>% 
  names()

# navigate to the target url
browser$navigate(url = "https://luzpar.netlify.app")


# click on the search icon
# Note that we have to define the element agin to use it!
search_icon <- browser$findElement(using = "css selector",
                                   value = ".fa-search")

search_icon$clickElement()


# find the search bar
search_bar <- browser$findElement(using = "css selector",
                                  value = "#search-query")

# click on it 
search_bar$clickElement()


# R might be faster than the browser, so slow it down
Sys.sleep(2)

# type something in
search_bar$sendKeysToElement(list(value = "law",
                                  key = "enter"))


# clear the search bar
search_bar$clearElement()




# switching between frames ------------------------------------------------

browser$navigate(url = "https://luzpar.netlify.app/documents/")

# wait for the page to be loaded...
Sys.sleep(10)

# find the frame, which is an element
# note: the frame features a shiny app that lives 
# originally at https://resulumit.shinyapps.io/luzpar/
# so we have to switch frames!
app_frame <- browser$findElement(using = "css selector",
                                 value = "iframe")

# switch it
browser$switchToFrame(Id = app_frame)

# switch back to the default frame
browser$switchToFrame(Id = NULL)





# solving a task ----------------------------------------------------------

# we need to scrape documents form this dynamic page

# 1) navigate to the target page and wait until it is loaded
browser$navigate(url = "https://luzpar.netlify.app/documents/")
Sys.sleep(10)


# 2) switch to the frame with the app
app_frame <- browser$findElement(using = "css selector",
                                 value = "iframe")

browser$switchToFrame(Id = app_frame)


# 3) find and open the drop down menu
drop_down <- browser$findElement(using = "css selector",
                                 value = ".filter-option-inner-inner")

drop_down$clickElement()


# 4) choose proposals
proposals <- browser$findElement(using = "css selector",
                                 value = "#bs-select-1-1")

proposals$highlightElement()
proposals$clickElement()


# 5) choose reports
report <- browser$findElement(using = "css selector",
                              value = "#bs-select-1-2")

report$highlightElement()
report$clickElement()


# 6) close the drop down menu
drop_down$clickElement()


# 7) get the page source and separte the links
links <- browser$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_elements("td a") %>% 
  html_attr("href")


# 8) write a for-loop and download the PDFs

library(glue)

pdf_link <- c()

for (i in 1:length(links)) {
  
  # show porgress
  cat(glue("Download {i}: "))
  
  # get the PDF-links
  pdf_link <- bow(url = links[i]) %>% 
    scrape() %>% 
    html_elements(css = ".btn-page-header") %>% 
    html_attr("href") %>% 
    # create the absolute URL for downloading the files
    url_absolute(base = "https://luzpar.netlify.app/")
  
  # save the files
  download.file(url = pdf_link,
                destfile = glue("Web_scraping/dynamic/output/{basename(pdf_link)}"))
}



# close browser -----------------------------------------------------------
browser$closeall()
