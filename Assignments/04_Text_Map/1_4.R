# load libraries
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)

# load miserables book
mis_book <- readRDS('data/miserables_book.rds')

# extract all words without stop words
words <- mis_book %>%
  unnest_tokens(word, text, token = 'words') %>%
  anti_join(stop_words, by = 'word') 
head(words)

# extract negative and positive emotion of book
emotion <- words %>%
  group_by(gutenberg_id) %>%
  dplyr:: mutate(order = row_number()) %>%
  inner_join (get_sentiments( "bing" )) %>%
  mutate(index = floor(order/ 200 )) %>%
  group_by(gutenberg_id, index, sentiment) %>%
  dplyr:: count() %>%
  tidyr:: spread(sentiment, n, fill = 0 ) %>%
  mutate(negative = -negative)

# bar plot 
ggplot(emotion, aes( x = index )) +
  geom_bar(aes( y = positive ), 
           stat="identity", color='hotpink1') + 
  geom_bar(aes( y = negative ), 
           stat="identity", color='cornflowerblue') +
  theme( axis.title.x=element_blank(),
         axis.title.y=element_blank() )
