#devtools::install_github("ropensci/gutenbergr")
library(dplyr)
library(gutenbergr)

#################   Dickens Books   #######################
# download dickens books
dickens_books <- gutenberg_works(author == 'Dickens, Charles') %>%
  gutenberg_download(meta_fields = 'title')

# extract novels from dickens books
novels <- c('A Tale of Two Cities', 'The Mystery of Edwin Drood', 
            'The Pickwick Papers', 'The Old Curiosity Shop', 
            'Oliver Twist', 'David Copperfield',
            'Hard Times', 'Dombey and Son', 'Our Mutual Friend', 
            'Barnaby Rudge: A Tale of the Riots of \'Eighty', 'Little Dorrit', 
            'Nicholas Nickleby', 'Martin Chuzzlewit', 
            'Bleak House', 'Great Expectations')
novels_match <- paste0("^", novels, "$", collapse = "|")
condition <- grepl(novels_match, dick_books$title)
books <- subset(dick_books, condition)

# save it to rds
saveRDS(books, 'data/dickens_books.rds')

####################   les miserable Books   #########################
# download miserable book
gutenberg_works() %>%
  filter(author == "Hugo, Victor") %>%
  summarise(gutenberg_id, title)

# gutenberg_id of les miserable series
ids = c(48731:48735)
miserables_book <- gutenberg_download(ids)

# save it to rds
saveRDS(miserables_book, 'data/miserables_book.rds')

####################   Robinson Books   #########################
# get ids of Robinson books
gutenberg_works() %>%
  filter(author == "Robinson, Frank M.") %>%
  summarise(gutenberg_id, title)

# gutenberg_id of Robinson books
ids = c(29680,32680,51008,51170,51268,51483)
robinson_books <- gutenberg_download(ids, meta_fields = 'title')

# save it to rds
saveRDS(robinson_books, 'data/robinson_books.rds') 
