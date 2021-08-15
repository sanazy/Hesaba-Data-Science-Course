# load libraries
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)

# load books
books <- readRDS('data/dickens_books.rds')

# extract words
words <- books %>%
  unnest_tokens(word, text, token = 'words') %>%
  anti_join(stop_words, by = "word" )
head(words)

# extract top20 words
top20 <- words %>%
  filter( ! is.na (word)) %>%
  group_by(word) %>%
  dplyr:: count () %>%
  arrange( desc (n)) %>%
  head(n = 20) 

# barplot of top 20 words
ggplot( top20, aes(reorder(word, n), n, fill=word) ) +
  geom_bar( stat = 'identity', col = "transparent") +
  geom_text( aes(y = n + 10, label = word), hjust = "left" ) +
  coord_flip() +
  labs( y = NULL , x = NULL ) +
  theme_minimal() + 
  theme( legend.position = 'none',
         axis.ticks.y = element_blank (),
         axis.text.y = element_blank () )
  
