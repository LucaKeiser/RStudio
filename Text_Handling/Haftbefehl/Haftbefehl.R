
# load packages -----------------------------------------------------------

library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.corpora)

library(tidyverse)
library(scales)




# read in lyrics ----------------------------------------------------------

lyrics <- readtext::readtext("Text_Handling/Haftbefehl/Lyrics/")

# take a look
class(lyrics)
typeof(lyrics)

lyrics[[1]]
# OR
lyrics %>% 
  pull(doc_id)




# create corpus -----------------------------------------------------------

lyrics_corpus <- corpus(lyrics)

class(lyrics_corpus)
typeof(lyrics_corpus)
summary(lyrics_corpus)




# create tokens -----------------------------------------------------------

lyrics_tokens <- quanteda::tokens(lyrics_corpus,
                                  what = "word",
                                  remove_punct = TRUE,
                                  remove_symbols = TRUE,
                                  remove_numbers = FALSE,
                                  remove_separators = TRUE)
# remove stopwords
lyrics_tokens <- lyrics_tokens %>% 
  tokens_remove(pattern = word(c(stopwords("de"), 
                                 "pu", "n", "aus'm", "mei'm", "schon", "wer", "cho", "her", "mal")),
                valuetype = 'fixed')


class(lyrics_tokens)
typeof(lyrics_tokens)
summary(lyrics_tokens)

# take a look at all tokens of a list element
lyrics_tokens[[1]]
lyrics_tokens[[4]]




# create a document-feature matrix (dfm) ----------------------------------

# NOTE
# every row represents a document
# every column represents a token
# ever cell represents a frequency
lyrics_dfm <- quanteda::dfm(lyrics_tokens)

# stemmed!
# unique words only
lyrics_dfm_wordstem <- quanteda::dfm_wordstem(lyrics_dfm)

class(lyrics_dfm_wordstem)
typeof(lyrics_dfm_wordstem)
summary(lyrics_dfm_wordstem)

# helper functions
quanteda::topfeatures(lyrics_dfm_wordstem)
quanteda::colSums(lyrics_dfm_wordstem)
quanteda::featnames(lyrics_dfm_wordstem)
quanteda::docnames(lyrics_dfm_wordstem)
quanteda::rowSums(lyrics_dfm_wordstem)




# descriptive analysis ----------------------------------------------------

## frequency --------------------------------------------------------------

freq <- quanteda.textstats::textstat_frequency(lyrics_dfm_wordstem)

# take a look
freq


freq %>% 
  as_tibble() %>% 
  # filter(feature != "cop-killa") %>% 
  arrange(desc(frequency)) %>% 
  head(50) %>% 
  mutate(feature = as_factor(feature),
         feature = fct_reorder(feature, frequency)) %>% 
  ggplot(aes(feature, frequency)) +
  geom_col(fill = "red",
           alpha = 0.5) +
  coord_flip() +
  labs(
      title = "Haftbefehl - Lyrics",
      subtitle = "Most common words\nhttps://www.redbull.com/de-de/haftbefehl-beste-song\n",
      x = "",
      y = "Count"
      ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal()




## wordcloud --------------------------------------------------------------

set.seed(12345)
quanteda.textplots::textplot_wordcloud(lyrics_dfm_wordstem,
                                       max_words = 100,
                                       min_size = 2,
                                       max_size = 8)





## lexical density --------------------------------------------------------

lyrics_ld <- as_tibble(quanteda.textstats::textstat_lexdiv(lyrics_dfm_wordstem, 
                                                           measure = "TTR"))
lyrics_ld


# clean up labels
labels <- lyrics_ld$document

labels <- labels %>% 
  str_remove_all(".pdf") %>% 
  str_replace_all("_", " ")

lyrics_ld$document <- labels


# plot
lyrics_ld %>% 
  mutate(document = fct_reorder(document, TTR)) %>% 
  ggplot(aes(document, TTR)) +
  geom_col(fill = "red",
           alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Limp Bizkit - Lyrics",
    subtitle = "Lexical Densitiy Socre (range from 0 to 1)",
    x = "",
    y = "Score"
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal()

lyrics_dfm_wordstem %>% 
  view()




## similarity -------------------------------------------------------------

lyrics_similarity <- quanteda.textstats::textstat_simil(lyrics_dfm_wordstem, margin = "documents", method = "correlation")

# convert to a matrix
lyrics_similarity <- as.matrix(lyrics_similarity)

# clean up names
rownames <- rownames(lyrics_similarity)

rownames <- rownames %>% 
  str_remove(".pdf") %>% 
  str_replace_all("_", " ")

# rownames == colnames
rownames(lyrics_similarity) <- rownames
colnames(lyrics_similarity) <- rownames


# base R
cor(lyrics_similarity)

# corrplot
corrplot::corrplot(lyrics_similarity,
                   type = "lower",
                   method = "circle")


# ggcorrplot
ggcorrplot::ggcorrplot(cor(lyrics_similarity),
                       type = "lower",
                       method = "circle")


