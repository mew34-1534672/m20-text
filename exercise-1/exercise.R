# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
# setwd("~/Desktop/INFO 201/m20-text/exercise-1")

# Load booksinto a dataframe using the austen_books() function
original.books <- austen_books()

# How many books are in the dataset?
number.books <- length(unique(original.books$book))

# Which book has the most lines?
book.max.lines <- original.books %>% 
                  group_by(book) %>% 
                  summarize(lines = n())
# Emma does, with 16,235

# Use the unnest_tokens function to generate the full list of words
all.words <- original.books %>% 
             unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
common.word <- all.words %>% 
               group_by(word) %>% 
               summarize(count = n()) %>% 
               arrange(-count)

# Remove stop words by performing an anti_join with the stop_words dataframe
no.stop.words <- all.words %>% 
  anti_join(stop_words, by="word")

# Which non stop-words are most common?
no.stop.common.words <- no.stop.words %>% 
                        group_by(word) %>% 
                        summarize(count = n()) %>% 
                        arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
no.stop.words %>% 
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
