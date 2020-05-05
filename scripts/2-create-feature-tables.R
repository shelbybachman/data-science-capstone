# this script creates feature frequency tables
# for shelby's data science capstone project
# shelby bachman, 2019
# last updated 02.03.2020

###### setup
# clear workspace
rm(list = ls())

# load packages
library(rprojroot)
library(R.utils)
library(tm)
library(quanteda)
library(dplyr)
library(stringr)

# source files within this project
path <- function(x) find_root_file(x, criterion = has_file('data-science-capstone.Rproj'))

##### create data for model (TRAINING SET)

# load data file
#nSubsets <- 5
subsets_to_choose <- c(3, 7, 18, 23, 26, 29)

# join all tokens into one large set of tokens
for (ii in subsets_to_choose) {
  
  filename_toRead <- paste(path('data/data_tokenized/subset_'), ii, '_tokenized.RData', sep = '')
  load(filename_toRead)
  
  if (ii == subsets_to_choose[1]) {
    all_tokens <- this_corpus_tokens
  } else {
    all_tokens <- c(all_tokens, this_corpus_tokens)
  }
  
}

rm(this_corpus_tokens)

# create document feature matrices for ngrams 1-3
this_dfm_unigram <- dfm(all_tokens, ngrams = 1)
this_dfm_bigram <- dfm(all_tokens, ngrams = 2)
this_dfm_trigram <- dfm(all_tokens, ngrams = 3)
this_dfm_quadgram <- dfm(all_tokens, ngrams = 4)
#this_dfm_pentagram <- dfm(all_tokens, ngrams = 5, remove = stopwords("english"))
#this_dfm_sextagram <- dfm(all_tokens, ngrams = 6, remove = stopwords("english"))

rm(all_tokens)

# compute frequency of each feature in each matrix, and split into component words
# and reduce dataframe sizes by removing items with frequency = 1
this_freq_unigram <- textstat_frequency(this_dfm_unigram) %>%
  select(feature, frequency) %>% 
  filter(frequency > 1)
this_freq_bigram <- textstat_frequency(this_dfm_bigram) %>%
  select(feature, frequency) %>% 
  filter(frequency > 1)
this_freq_trigram <- textstat_frequency(this_dfm_trigram) %>%
  select(feature, frequency) %>% 
  filter(frequency > 1)
this_freq_quadgram <- textstat_frequency(this_dfm_quadgram) %>%
  select(feature, frequency) %>% 
  filter(frequency > 1)
#this_freq_pentagram <- textstat_frequency(this_dfm_pentagram) %>%
#  select(feature, frequency) %>% 
#  filter(frequency > 1)
#this_freq_sextagram <- textstat_frequency(this_dfm_sextagram) %>%
#  select(feature, frequency) %>% 
#  filter(frequency > 1)

rm(this_dfm_unigram, this_dfm_bigram, this_dfm_trigram, this_dfm_quadgram)#, this_dfm_pentagram, this_dfm_sextagram)

save(this_freq_unigram, file = path('data/data_dfm/freq_unigram.RData'))
rm(this_freq_unigram)

# create additional columns in bigram table
this_freq_bigram <- this_freq_bigram %>%
  rowwise() %>%
  mutate(start = str_split(feature, '_')[[1]][1],
         end = str_split(feature, '_')[[1]][2])

save(this_freq_bigram, file = path('data/data_dfm/freq_bigram.RData'))
rm(this_freq_bigram)

# create additional columns in trigram table
this_freq_trigram <- this_freq_trigram %>%
  rowwise() %>%
  mutate(first = str_split(feature, '_')[[1]][1],
         second = str_split(feature, '_')[[1]][2],
         start = paste(first, second, sep = ' '),
         end = str_split(feature, '_')[[1]][3]) %>%
  select(feature, frequency, start, end)

save(this_freq_trigram, file = path('data/data_dfm/freq_trigram.RData'))
rm(this_freq_trigram)

this_freq_quadgram <- this_freq_quadgram %>%
  rowwise() %>%
  mutate(first = str_split(feature, '_')[[1]][1],
         second = str_split(feature, '_')[[1]][2],
         third = str_split(feature, '_')[[1]][3],
         start = paste(first, second, third, sep = ' '),
         end = str_split(feature, '_')[[1]][4]) %>%
  select(feature, frequency, start, end)

save(this_freq_quadgram, file = path('data/data_dfm/freq_quadgram.RData'))
rm(this_freq_quadgram)

# this_freq_pentagram <- this_freq_pentagram %>%
#   rowwise() %>%
#   mutate(first = str_split(feature, '_')[[1]][1],
#          second = str_split(feature, '_')[[1]][2],
#          third = str_split(feature, '_')[[1]][3],
#          fourth = str_split(feature, '_')[[1]][4],
#          start = paste(first, second, third, fourth, sep = ' '),
#          end = str_split(feature, '_')[[1]][5]) %>%
#   select(feature, frequency, start, end)
# 
# this_freq_sextagram <- this_freq_sextagram %>%
#   rowwise() %>%
#   mutate(first = str_split(feature, '_')[[1]][1],
#          second = str_split(feature, '_')[[1]][2],
#          third = str_split(feature, '_')[[1]][3],
#          fourth = str_split(feature, '_')[[1]][4],
#          fifth = str_split(feature, '_')[[1]][5],
#          start = paste(first, second, third, fourth, fifth, sep = ' '),
#          end = str_split(feature, '_')[[1]][6]) %>%
#   select(feature, frequency, start, end)

#save(this_freq_pentagram, file = path('data/data_dfm/freq_pentagram.RData'))
#save(this_freq_sextagram, file = path('data/data_dfm/freq_sextagram.RData'))


##### create data for model (TEST SET)

# load data file
subset_to_choose <- 14

# load tokenized data
filename_toRead <- paste(path('data/data_tokenized/subset_'), subset_to_choose, '_tokenized.RData', sep = '')
load(filename_toRead)

# create document feature matrices for ngrams 3-4
test_dfm_trigram <- dfm(this_corpus_tokens, ngrams = 3)
test_dfm_quadgram <- dfm(this_corpus_tokens, ngrams = 4)
rm(this_corpus_tokens)

# compute frequency of each feature in each matrix, 
# select only moderately frequent entries for testing
test_freq_trigram <- textstat_frequency(test_dfm_trigram) %>%
  select(feature, frequency) %>% 
  filter(frequency > 10)
test_freq_quadgram <- textstat_frequency(test_dfm_quadgram) %>%
  select(feature, frequency) %>% 
  filter(frequency > 10)

rm(test_dfm_trigram, test_dfm_quadgram)

# create additional columns in trigram table
test_freq_trigram <- test_freq_trigram %>%
  rowwise() %>%
  mutate(first = str_split(feature, '_')[[1]][1],
         second = str_split(feature, '_')[[1]][2],
         start = paste(first, second, sep = ' '),
         end = str_split(feature, '_')[[1]][3]) %>%
  select(feature, frequency, start, end)

save(test_freq_trigram, file = path('data/data_dfm/freq_trigram_test.RData'))
rm(test_freq_trigram)

test_freq_quadgram <- test_freq_quadgram %>%
  rowwise() %>%
  mutate(first = str_split(feature, '_')[[1]][1],
         second = str_split(feature, '_')[[1]][2],
         third = str_split(feature, '_')[[1]][3],
         start = paste(first, second, third, sep = ' '),
         end = str_split(feature, '_')[[1]][4]) %>%
  select(feature, frequency, start, end)

save(test_freq_quadgram, file = path('data/data_dfm/freq_quadgram_test.RData'))
rm(test_freq_quadgram)
