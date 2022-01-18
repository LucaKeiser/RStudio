# load packages
library(tidyverse)
library(quanteda)



# readr::guess_encoding ---------------------------------------------------

# get the right encoding
spiegel_files <- list.files(here::here("data"), 
                            full.names = TRUE)

# Building a for loop to repeat the execution for each file
# Initialize the containers
encoding <- vector(mode = "character", length(spiegel_files))
confidence <- vector(mode = "numeric", length(spiegel_files))

for (i in 1:length(spiegel_files)) {
  
  # Displays iterations
  cat(i," ")
  
  # Storing guessed encoding at file[i]:
  tmp <- readr::guess_encoding(spiegel_files[i])
  
  if (nrow(tmp) > 0) {     # When multiple guesses, keep one with max confidence
    best_enc <- which.max(tmp$confidence)
    encoding[i] <- tmp$encoding[best_enc]
    confidence[i] = tmp$confidence[best_enc]
  } else {                 # When no guess is returned, 0
    encoding[i] <- NA
    confidence[i] = 0
  }
  rm(tmp)
}


# read in the data with the right encoding
spiegel_texts <- readtext::readtext(here::here("data"),
                                    encoding = encoding)


# check
writeLines(spiegel_texts$text[1])



# create text corpus ------------------------------------------------------

spiegel_corpus <- corpus(spiegel_texts) 


### cleaning

# split lines
spiegel_corpus_vec <- spiegel_corpus %>% 
  str_split("\n")

# remove header and footer
for (i in 1:length(spiegel_corpus_vec)) {
  # iterate through the list and delete the first 430 (assumption!) lines
  spiegel_corpus_vec[[i]] <- spiegel_corpus_vec[[i]][-1:-430]
  # define the length of each file
  a <- length(spiegel_corpus_vec[[i]])
  # remove the last 200 (assumption!) lines
  b <- a-200
  # overwrite the file
  spiegel_corpus_vec[[i]] <- spiegel_corpus_vec[[i]][-b:-a]
}


# check length of the files
temp <- vector(mode = "numeric", length = length(spiegel_corpus_vec))

for (i in 1:length(spiegel_corpus_vec)) {
  temp[i] <- length(spiegel_corpus_vec[[i]])
}

temp
sort(temp)

# take a look at the shortest file
spiegel_corpus_vec[[139]]

# take a look at the longest file
spiegel_corpus_vec[[105]]


# unite again
spiegel_corpus <- spiegel_corpus_vec %>% 
  str_c()



# create tokens -----------------------------------------------------------

spiegel_tokens <- tokens(spiegel_corpus, 
                         what = "word",
                         remove_punct = TRUE,
                         remove_numbers = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE)

# cleaning 2.0
spiegel_tokens_clean <- spiegel_tokens %>% 
  tokens_tolower() %>% 
  tokens_remove(c(stopwords("de"), "whatsapp", "send", "text", "28der", "dass", "symbolbild",
                "link", "kopieren", "yumeng", "beitrag", "spiegel", "share", "fb-messenger", "app_id",
                "e-mail", "veröffentlicht", "hinzufügen", "merkliste", "mehr", "cover", "sagt", "artikel",
                "bild", "foto", "seit", "schon", "immer", "edit", "shop", "facebook", "titelgeschicht")) %>%
  tokens_remove(c("^http", "^.$", "^..$", "^...$", "^2f", "^bento", "^20",
                  "^w[u|o]r", "^gutsch", "^sei"), valuetype = "regex")
                  
# take a look
head(spiegel_tokens_clean)
# shortest article
spiegel_tokens_clean[[139]]
# longest article 
spiegel_tokens_clean[[105]]



# create dfm --------------------------------------------------------------

spiegel_dfm <- quanteda::dfm(spiegel_tokens_clean)

# stemming
spiegel_dfm_wordstem <- quanteda::dfm_wordstem(spiegel_dfm)


# take a look
summary(spiegel_dfm_wordstem)
ndoc(spiegel_dfm_wordstem)
nfeat(spiegel_dfm_wordstem)
featnames(spiegel_dfm_wordstem)
topfeatures(spiegel_dfm_wordstem)



# textplots ---------------------------------------------------------------

# rename docnames
spiegel_files <- spiegel_files %>% 
  str_remove_all("C:/Users/LucaK/Desktop/Spiegel/data/") %>% 
  str_remove_all(".txt") %>% 
  lubridate::as_date() %>% 
  as.character()

# add to dfm-object
spiegel_dfm_wordstem@docvars$docname_ <- spiegel_files

# create frequency
freq <- quanteda.textstats::textstat_frequency(spiegel_dfm_wordstem)



# plot feature frequency
freq %>% 
  as_tibble() %>% 
  arrange(desc(frequency)) %>% 
  head(50) %>% 
  mutate(feature = as_factor(feature),
         feature = fct_reorder(feature, frequency)) %>% 
  ggplot(aes(feature, frequency)) +
  geom_col(fill = "red",
           alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Spiegel",
    subtitle = "Most common words",
    x = "",
    y = "Count"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal()


# wordcloud
quanteda.textplots::textplot_wordcloud(spiegel_dfm_wordstem, min_size = 1, max_size = 3, max_words = 200)




# plotting by group (over time) -------------------------------------------

freq_over_time <- quanteda.textstats::textstat_frequency(spiegel_dfm_wordstem,
                                                         groups = docname_)


# 1. plot
freq_over_time %>% 
  filter(feature %in% c("protest")) %>% 
  ggplot(aes(x = lubridate::as_date(group), y = frequency)) + 
  geom_col(aes(fill = feature),
           alpha = 0.75) +
  geom_smooth(aes(color = feature), 
              se = FALSE) +
  theme_light() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none") +
  labs(title = "Frequency Analysis over Time",
       subtitle = "Word: 'Protest'",
       x = "",
       y = "Frequency")



# 2. plot
freq_over_time %>% 
  filter(feature %in% "impfung") %>% 
  ggplot(aes(x = lubridate::as_date(group), y = frequency)) + 
  geom_col(aes(fill = feature),
           alpha = 0.75) +
  geom_smooth(aes(color = feature), 
              se = FALSE) +
  theme_light() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none") +
  labs(title = "Frequency Analysis over Time",
       subtitle = "Word: 'Impfung'",
       x = "",
       y = "Frequency")



# 3. plot
freq_over_time %>% 
  filter(feature %in% "masken") %>% 
  ggplot(aes(x = lubridate::as_date(group), y = frequency)) + 
  geom_col(aes(fill = feature),
           alpha = 0.75) +
  geom_smooth(aes(color = feature), 
              se = FALSE) +
  theme_light() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none") +
  labs(title = "Frequency Analysis over Time",
       subtitle = "Word: 'Masken'",
       x = "",
       y = "Frequency")



# 4. plot
freq_over_time %>% 
  filter(feature %in% c("protest", "impfung", "masken")) %>% 
  ggplot(aes(x = lubridate::as_date(group), y = frequency)) + 
  geom_col(aes(fill = feature),
           alpha = 0.75, 
           position = "dodge") +
  geom_smooth(aes(color = feature),
              se = FALSE) +
  theme_light() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Frequency Analysis over Time",
       x = "",
       y = "Frequency",
       fill = "Word:",
       color = "Word:")



# rename (grouping) features ----------------------------------------------

freq_over_time_rec <- freq_over_time %>% 
  mutate(feature_rec = case_when(str_detect(feature, "impfung") ~ "impfung",
                                 str_detect(feature, "protest") ~ "protest",
                                 str_detect(feature, "maske") ~ "maske",
                                 TRUE ~ feature))



# 5. plot
freq_over_time_rec %>% 
  filter(feature_rec %in% c("protest", "impfung", "maske")) %>% 
  group_by(group, feature_rec) %>% 
  summarise(count = sum(frequency)) %>%
  # remove outliers
  filter(count < 60) %>% 
  ggplot(aes(x = lubridate::as_date(group), y = count)) + 
  geom_col(aes(fill = feature_rec),
           position  = "dodge") +
  geom_smooth(aes(group = feature_rec, color = feature_rec),
              se = FALSE) +
  theme_light() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Frequency Analysis over Time",
       subtitle = "ALL Words which containe 'Impfung', 'Maske' or 'Protest'",
       y = "Frequency",
       x = "",
       color = "Word:",
       fill = "Word:")
