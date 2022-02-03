# load packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(RSelenium)
library(polite)
library(glue)
library(lubridate)
library(ggiraph)
library(plotly)


# check if it is okay to scrape first -------------------------------------

base_url <- "https://www.spotrac.com/epl/rankings/"
bow(url = base_url)


# get all urls ------------------------------------------------------------

endings <- as.character(2011:2021)

urls_salary <- url_absolute(endings, base_url)
urls_contract_value <- str_c(urls_salary, "/contract-value/")
urls_transfer_fee <- str_c(urls_salary, "/transfer-fee/")

all_urls <- c(urls_salary, urls_contract_value, urls_transfer_fee)



# start a server ----------------------------------------------------------

remote_server <- rsDriver(browser="firefox")

# separate client and server
browser <- remote_server$client
server <- remote_server$server

browser$maxWindowSize()



# write for loop and scrape -----------------------------------------------

# reject all cookies button has to be pressed manually...

browser$navigate(url = all_urls[1])


# create empty tibble to store the output
df_final <- tibble(
  "year" = character(),
  "player" = character(),
  "position" = character(),
  "team" = character(),
  "amount_pounds" = character()
)


for (i in all_urls){
  
  cat("\n")
  cat(glue("Downloading form ***{i}***. Please wait..."))
  cat("\n")
  
  # visit
  browser$navigate(url = i)
  Sys.sleep(3)
  
  # scrape page
  page <- bow(browser$getCurrentUrl()[[1]]) %>% 
    scrape()
  
  # get the data and store it in a tibble
  df <- tibble(
    "year" = page %>% 
      html_element(css = "h2") %>% 
      html_text(),
    "player" = page %>% 
      html_elements(css = ".team-name") %>% 
      html_text(),
    "position" = page %>% 
      html_elements(css = ".rank-position") %>% 
      html_text(),
    "team" = page %>% 
      html_elements(css = ".noborderright+ td.center") %>% 
      html_text(),
    "amount_pounds" = page %>% 
      html_elements(css = ".info") %>% 
      html_text()
  )
  Sys.sleep(2)
  
  # create final tibble
  df_final <- rbind(df_final, df)

  }


# store data and close connection -----------------------------------------

write_rds(x = df_final,
          file = "Web_scraping/dynamic/output/df_final_soccer.rds")


# close connection --------------------------------------------------------

browser$close()
server$stop()

rm(list = ls())
gc()



# load data ---------------------------------------------------------------

df <- read_rds("Web_scraping/dynamic/output/df_final_soccer.rds")

df %>% head()

# cleaning
df_clean <- df %>% 
  separate(year, into = c("year", "title"), sep = "    ") %>%
  separate(year, into = c("year_from", "year_to"), sep = "-") %>% 
  mutate(amount_pounds = parse_number(amount_pounds)) %>% 
  mutate(amount_dollar = amount_pounds * 1.3) %>%
  mutate(across(.cols = c("year_from", "year_to"), ~as.numeric(.))) %>% 
  mutate(across(.cols = c("title", "player", "position", "team"), ~str_trim(.))) %>% 
  mutate(title = str_remove(title, " Rankings")) %>% 
  mutate(across(.cols = c("title", "position", "team"), ~as_factor(.))) %>% 
  select(-amount_pounds) %>% 
  relocate("title", "year_from", "year_to") %>% 
  mutate(full_team = case_when(
    team == "CFC" ~ "Chelsea",
    team == "MUFC" ~ "Manchester United",
    team == "AVFC" ~ "Aston Villa",
    team == "AFC" ~ "Arsenal",
    team == "MCFC" ~ "Manchester City",
    team == "THFC" ~ "Tottenham",
    team == "LC" ~ "Leicester City",
    team == "LFC" ~ "Liverpool",
    team == "SCAFC" ~ "Swansea City",
    team == "SCFC" ~ "Stoke City",
    team == "BOU" ~ "Bournemouth",
    team == "EFC" ~ "Everton",
    team == "NUFC" ~ "Newcastle United",
    team == "BHA" ~ "Brighton",
    team == "WHUFC" ~ "West Ham United",
    team == "CP" ~ "Crystal Palace",
    team == "SFC" ~ "Southampton"
    ))



# descriptive analysis ----------------------------------------------------

theme_set(theme_light())


# avg. overall
df_clean %>% 
  group_by(title) %>% 
  summarise(avg = mean(amount_dollar)) %>% 
  mutate(title = fct_reorder(title, avg)) %>% 
  ggplot(aes(x = title, y = avg, fill = title)) +
  geom_col() +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = seq(0, 40000000, by = 7500000)) +
  labs(title = "English Premier League (2011 - 2021)",
       subtitle = "Avarage $-Amount per Type",
       x = "Year",
       y = "") 



# plot 1: $-Amount Devlopment over Time per Type
df_clean %>% 
  ggplot(aes(x = year_from, y = amount_dollar)) + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess",
              color = "red") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
  labs(title = "English Premier League (2011 - 2021)",
       subtitle = "$-Amount Devlopment per Type",
       x = "Year",
       y = "") +
  facet_wrap(~ title)



# plot 2: $-Amount Development per Type and Position
df_clean %>% 
  ggplot(aes(x = year_from, y = amount_dollar)) +
  geom_point(alpha = 0.15)+ 
  geom_smooth(method = "loess",
              color = "red") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
  labs(title = "English Premier League (2011 - 2021)",
       subtitle = "$-Amount Development per Type and Position",
       x = "Year",
       y = "") +
  facet_grid(position ~ title)




# plot 3: Annual Salary Development per Team
df_clean %>% 
  filter(title == "Annual Salary",
         amount_dollar > 0) %>% 
  group_by(team) %>% 
  add_count() %>% 
  filter(n >= 15) %>% 
  mutate(full_team = fct_reorder(full_team, amount_dollar)) %>% 
  ggplot(aes(x = year_from, y = amount_dollar)) + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess",
              color = "red") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 1)) +
  labs(title = "English Premier League (2011 - 2021)",
       subtitle = "Annual Salary Development per Teams with n >= 15",
       x = "Year",
       y = "") +
  facet_wrap(~ full_team)




# plot 3 $-Amount Development per Type and Player mentioned >= 10 times

# create labels (tooltips)
df_clean$tooltip_1 <- c(paste0(df_clean$player, "\n", df_clean$full_team, "\n", df_clean$position, "\n", df_clean$amount_dollar))


# 3,1 ggiraph
gg_obj_1 <- df_clean %>% 
  # filtered (to less data...)
  filter(title != "Contract Transfer Fee") %>% 
  group_by(title, player) %>% 
  add_count(player) %>% 
  filter(n >= 10) %>% 
  ungroup() %>% 
  mutate(player = fct_reorder(player, amount_dollar, max, .desc = TRUE)) %>% 
  ggplot(aes(x = year_from, y = amount_dollar)) + 
  geom_line(aes(color = player)) +
  geom_point_interactive(aes(tooltip = tooltip_1, color = player)) +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = seq(0, 155000000, by = 25000000)) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 3)) +
  theme(legend.position = "none") + 
  labs(title = "English Premier League (2011 - 2021)",
       subtitle = "$-Amount Development per Type and Player mentioned >= 10 times",
       x = "Year",
       y = "") +
  facet_wrap(~ title)


girafe(code = print(gg_obj_1),
       height = 3.5,
       width = 7)





# 3.2 plotly

gg_obj_2 <- df_clean %>% 
  filter(title != "Contract Transfer Fee") %>% 
  group_by(title, player) %>% 
  add_count(player) %>% 
  filter(n >= 10) %>% 
  ungroup() %>% 
  mutate(player = fct_reorder(player, amount_dollar, max, .desc = TRUE)) %>% 
  ggplot(aes(x = year_from, y = amount_dollar)) + 
  geom_line(aes(color = player)) +
  # use the text argument to specify the labels!
  # tooltip_1 can be reused in this case!
  geom_point(aes(color = player, text = tooltip_1)) +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = seq(0, 155000000, by = 25000000)) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 3)) +
  theme(legend.position = "none") +
  labs(title = "English Premier League (2011 - 2021)",
       subtitle = "$-Amount Development per Type and Player mentioned >= 10 times",
       x = "Year",
       y = "",
       color = "") +
  facet_wrap(~ title)
  

# Note: subtitle gets lost. Subtitle fix does not look pretty... 
p <- ggplotly(gg_obj_2,
         tooltip = "text")
