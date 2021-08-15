# load libraries
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(tidytext)
library(wordcloud2)

# load books
books <- readRDS('data/dickens_books.rds')

# extract all words
words <- books %>%
  unnest_tokens(word, text, token = 'words') %>%
  anti_join(stop_words, by = "word" )
head(words)

# extract top 200 words
top200 <- words %>%
  filter( !is.na(word) ) %>%
  group_by(word) %>%
  dplyr:: count() %>%
  arrange(desc(n)) %>%
  mutate(freq = sqrt(n)) %>%
  summarise(word, n) %>%
  head(n = 200)

# wordcloud
figPath = system.file('examples/dick.jpg', package = 'wordcloud2')
wordcloud2(top200, figPath = figPath, size = 0.2)
