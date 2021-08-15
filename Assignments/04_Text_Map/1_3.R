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

# extract unique book titles
titles <- books$title %>% unique()

# create empty dataframe
allnames <- data.frame()

for (tit in titles){
  # filter books by specific title
  book <- books %>%
    filter(title == tit)
  
  # separate all capitalized words
  caps = str_extract(book$text, "[A-Z][a-z]+") %>%
    unlist() 
  
  # separate all words
  all <- str_extract_all(book$text, "[A-Za-z]+")  %>%
    unlist() 
  
  # extract words that are capitalized through book
  names <- caps[!str_to_lower(caps) %in% all]
  
  # dataframe with top 10 names and their frequencies
  df <- as.data.frame(table(names)) %>% 
    mutate(title = tit) %>%
    arrange(desc(Freq)) %>% 
    slice(1:7)
  
  # bind dataframe to allnames dataframe
  allnames <- rbind(allnames, df)
}

# filter these notnames words from dataframe
notnames <- c('Mr', 'Mrs', 'London', 'France', 'Sydney', 'Chapter')
allnames <- allnames %>%
  filter(!(names %in% notnames))

# bar plot
ggplot(allnames, aes(x = reorder(names, Freq), y = Freq, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "most used names") +
  facet_wrap(~title, ncol = 4, scales = "free") +
  coord_flip()
