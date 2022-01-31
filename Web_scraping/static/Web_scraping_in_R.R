# Source: https://www.youtube.com/watch?v=NwtxrbqE2Gc



# load packages -----------------------------------------------------------

library(rvest)
library(tidyverse)



# get familiar with rvest -------------------------------------------------

# define the webpage

reddit_webpage <- read_html("https://www.reddit.com/r/politics/comments/a1j9xs/partisan_election_officials_are_inherently_unfair/")


# 1) 

reddit_webpage %>% 
  html_node("title") %>% 
  html_text()

# OR with html_elements and css-selectors!

reddit_webpage %>% 
  html_elements(css = "._29WrubtjAcKqzJSPdQqQ4h > h1:nth-child(1)") %>% 
  html_text()


# 2) 

reddit_webpage %>% 
  html_nodes("p._1qeIAgB0cPwnLhDF9XSiJM") %>% 
  html_text()


reddit_webpage %>% 
  html_elements(css = "._1qeIAgB0cPwnLhDF9XSiJM") %>% 
  html_text()



# work with rvest ---------------------------------------------------------

reddit_political_news <- read_html("https://www.reddit.com/r/politics/new/")


### get the time stamps 

reddit_political_news %>% 
  html_nodes("a._3jOxDPIQ0KaOWpzvSQo-1s") %>% 
  html_text()

# OR

time <- reddit_political_news %>% 
  html_elements(css = "._3jOxDPIQ0KaOWpzvSQo-1s") %>% 
  html_text()



### get the urls of the comments

reddit_political_news %>% 
  html_nodes("a._3jOxDPIQ0KaOWpzvSQo-1s") %>% 
  html_attr("href")

# OR

urls <- reddit_political_news %>% 
  html_elements(css = "._3jOxDPIQ0KaOWpzvSQo-1s") %>% 
  html_attr("href")



### transfrom to a tibble

reddit_newspage_times <- tibble(
  news_page = urls,
  published_time = time
)



### loop through all the urls and get the titles and comments

# create empty vector for storage
titles <- c()
comments <- c()

# at the end we want a tibble which contains every comment for every article

# article 1     comment 1
# article 1     comment 2
# article 1     comment 3
# article 2     comment 1
# article 2     comment 2 

# etc.

for (i in reddit_newspage_times$news_page) {
  
  # 1) 
  reddit_newspage_times <- read_html(i)
  body <- reddit_newspage_times %>% 
    html_elements(css = "._1qeIAgB0cPwnLhDF9XSiJM") %>% 
    html_text()
  
  comments <- append(comments, body)

  # 2)
  reddit_newspage_times <- read_html(i)
  title <- reddit_newspage_times %>% 
    html_elements(css = "._29WrubtjAcKqzJSPdQqQ4h > h1:nth-child(1)") %>% 
    html_text()
  
  titles <- append(titles, rep(title, each = length(body)))

  }


# put into a tibble

reddit_data <- tibble(
  Headline = titles,
  Comments = comments
  )


reddit_data %>% 
  select(Comments) %>% 
  head()

# get rid of the disclaimer comments!

disclaimers <- c(
  "As a reminder, this subreddit is for civil discussion.",
  "In general, be courteous to others. Debate/discuss/argue the merits of ideas, don't attack people. Personal insults, shill or troll accusations, hate speech, any suggestion or support of harm, violence, or death, and other rule violations can result in a permanent ban.",
  "If you see comments in violation of our rules, please report them.",
  "For those who have questions regarding any media outlets being posted on this subreddit, please click here to review our details as to our approved domains list and outlet criteria.",
  "I am a bot, and this action was performed automatically. Please contact the moderators of this subreddit if you have any questions or concerns."
)

reddit_data_filtered <- reddit_data %>% 
  filter(!Comments %in% disclaimers)

# check
reddit_data_filtered %>% 
  select(Comments) %>% 
  head()



# sentiment analysis with sentimentr --------------------------------------

library(sentimentr)

# sentiment scores range form -1 to 1

# sentiment sores on a sentence level
sentiment_scores <- sentiment(reddit_data_filtered$Comments)

# sentiment scores for per comment
sentiment_scores %>% 
  group_by(element_id) %>% 
  summarise(avg_per_comment = mean(sentiment))
  

# sentiment overall
sentiment_scores %>% 
  summarise(avg_overall = mean(sentiment))



final_df <- sentiment_scores %>% 
  group_by(element_id) %>% 
  mutate(avg_per_comment = mean(sentiment)) %>% 
  ungroup() %>% 
  mutate(avg_overall = mean(sentiment))





# create output-file ------------------------------------------------------

library(glue)
write_csv(x = final_df,
          file = glue("C:/Users/LucaK/Desktop/GitHub/RStudio/Web_scraping/static/output/output_{format(Sys.time(), '%Y_%m_%d_%H_%M')}.csv"))

