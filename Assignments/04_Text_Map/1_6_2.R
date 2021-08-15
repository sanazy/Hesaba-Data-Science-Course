# load libraries
library(dplyr)
library(stringr)
library(plyr)
library(tidyr)
library(tidytext)
library(ggplot2)

# load data
books <- readRDS('data/dickens_books.rds')

# separate each chapter of each book and label it 
chapterized <- books %>%
  filter(title == 'Oliver Twist') %>%
  dplyr::mutate(linenumber = row_number(),
                chapter = cumsum(str_detect(text, 
                                            regex("chapter", 
                                                  ignore_case = TRUE)))) %>%
  ungroup()

######################         1-grams          ######################

book_words <- chapterized %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  dplyr::count(chapter, word, sort = TRUE) %>%
  bind_tf_idf(word, chapter, n)

book_words %>%
  bind_tf_idf(word, chapter, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(chapter) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = chapter)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~chapter, ncol = 4, scales = "free") +
  coord_flip()

######################         2-grams          ######################
bigrams <- chapterized %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  dplyr::count(chapter, bigram) %>%
  bind_tf_idf(bigram, chapter, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(chapter) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = chapter)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~chapter, ncol = 4, scales = "free") +
  coord_flip()

######################       Zipf’s law - 1 gram         ######################
freq_by_rank <- book_words %>% 
  group_by(chapter) %>% 
  dplyr::mutate(rank = row_number(), 
                `term frequency` = n/sum(n))

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = chapter)) + 
  geom_abline(intercept = -1.6049, slope = -0.5564, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

######################       Zipf’s law - 2 gram         ######################
freq_by_rank <- bigram_tf_idf %>% 
  group_by(chapter) %>% 
  dplyr::mutate(rank = row_number(), 
                `term frequency` = n/sum(n))

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = chapter)) + 
  geom_abline(intercept = -2.191, slope = -0.161, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
