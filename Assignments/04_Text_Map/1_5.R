# load libraries
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# load books
books <- readRDS('data/dickens_books.rds')

# seperate bigrams from text into 2 columns
bigrams <- books %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') 

############### Verbs for Women #####################
# extract verbs after She/he
she_verbs <- bigrams %>%
  filter(word1 == 'She' | word1 == 'she') %>%
  filter(!word2 %in% stop_words$word)

# extract top 20 verbs for she
she_df <- as.data.frame(table(she_verbs$word2)) %>%
  arrange(desc(Freq)) %>%
  slice(1:20)

############### Verbs for Men #####################
# extract verbs after He/he
he_verbs <- bigrams %>%
  filter(word1 == 'He' | word1 == 'he') %>%
  filter(!word2 %in% stop_words$word)

# extract top 20 verbs for he
he_df <- as.data.frame(table(he_verbs$word2)) %>%
  arrange(desc(Freq)) %>%
  slice(1:20)

###############     Plots     #####################
# plot for she
g1 <- ggplot( she_df, 
              aes(reorder(Var1, Freq), Freq, fill = Var1) ) +
  geom_bar( stat = 'identity', col = "transparent") +
  geom_text( aes(label = Var1), hjust = "right" ) +
  coord_flip() +
  labs( y = NULL , x = 'Verbs used for Women' ) +
  theme_minimal() + 
  theme( legend.position = 'none',
         axis.ticks.y = element_blank (),
         axis.text.y = element_blank () )

# plot for he
g2 <- ggplot( he_df, 
              aes(reorder(Var1, Freq), Freq, fill = Var1) ) +
  geom_bar( stat = 'identity', col = "transparent") +
  geom_text( aes(label = Var1), hjust = "right" ) +
  coord_flip() +
  labs( y = NULL , x = 'Verbs used for Men' ) +
  theme_minimal() + 
  theme( legend.position = 'none',
         axis.ticks.y = element_blank (),
         axis.text.y = element_blank () )

# subplot
grid.arrange(g1, g2, ncol = 2, nrow = 1)
