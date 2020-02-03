# this script creates a prediction model
# for shelby's data science capstone project
# shelby bachman, 2019
# last updated 10.09.2019

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

##### functions

# this function creates

##### create data for model

# load data file
nSubsets <- 30

# join all tokens into one large set of tokens
for (ii in 1:nSubsets) {
  
  filename_toRead <- paste(path('data/data_tokenized/subset_'), ii, '_tokenized.RData', sep = '')
  load(filename_toRead)
  
  if (ii == 1) {
    all_tokens <- this_corpus_tokens
  } else {
    all_tokens <- c(all_tokens, this_corpus_tokens)
  }
  
}

# create document feature matrices
this_dfm_unigram <- dfm(all_tokens, ngrams = 1)
this_dfm_bigram <- dfm(all_tokens, ngrams = 2)
this_dfm_trigram <- dfm(all_tokens, ngrams = 3)

# compute frequency of each feature in each matrix
this_freq_unigram <- textstat_frequency(this_dfm_unigram) %>%
  select(feature, frequency)
this_freq_bigram <- textstat_frequency(this_dfm_bigram) %>%
  select(feature, frequency)
this_freq_trigram <- textstat_frequency(this_dfm_trigram) %>%
  select(feature, frequency)

##### TBA: need to add in more data nd bind


rm(this_dfm_unigram, this_dfm_bigram, this_dfm_trigram)

# create additional columns in bigram table
this_freq_bigram <- this_freq_bigram %>%
  rowwise() %>%
  mutate(start = str_split(feature, '_')[[1]][1],
         end = str_split(feature, '_')[[1]][2])

# create additional columns in trigram table
this_freq_trigram <- this_freq_trigram %>%
  rowwise() %>%
  mutate(first = str_split(feature, '_')[[1]][1],
         second = str_split(feature, '_')[[1]][2],
         start = paste(first, second, sep = ' '),
         end = str_split(feature, '_')[[1]][3]) %>%
  select(feature, frequency, start, end)


save(this_freq_unigram, file = path('data/data_dfm/freq_unigram.RData'))
save(this_freq_bigram, file = path('data/data_dfm/freq_bigram.RData'))
save(this_freq_trigram, file = path('data/data_dfm/freq_trigram.RData'))


##### test some inputs

# take a two-word input
# and return a new dataframe with all the trigrams
# that have the input as the first two words

input_word <- 'new york'
input_word <- 'what is'
input_word <- 'how are'

# A. calculate probabilities based on observed trigrams
observed_trigrams <- this_freq_trigram[this_freq_trigram$start %in% input_word, ]
observed_trigrams <- observed_trigrams %>%
  arrange(desc(frequency)) %>%
  select(end, frequency)

nObservedTrigrams <- sum(observed_trigrams$frequency)
discount_trigram <- 0.5 # trigram discounting parameter

probs_trigram <- observed_trigrams %>%
  mutate(prob = (frequency - discount_trigram) / nObservedTrigrams) %>%
  select(end, prob)

alpha_trigram <- 1 - sum(probs_trigram$prob)

# B. calculate probabilities based on observed bigrams
input_word_second = str_split(input_word, ' ')[[1]][2]
observed_bigrams <- this_freq_bigram[this_freq_bigram$start %in% input_word_second, ]
observed_bigrams <- observed_bigrams %>%
  arrange(desc(frequency)) %>%
  select(end, frequency)

nObservedBigrams <- sum(observed_bigrams$frequency)
discount_bigram <- 0.5 # bigram discounting parameter

probs_bigram <- observed_bigrams %>%
  mutate(prob = (frequency - discount_bigram) / nObservedBigrams) %>%
  select(end, prob)

alpha_bigram <- 1 - sum(probs_bigram$prob)

# C. unobserved bigrams
# unobserved bigrams could be (second input + any unigram), excluding those 

unobserved_bigrams <- this_freq_unigram[!this_freq_unigram$feature %in% observed_bigrams$end,]
nUnobservedBigrams <- sum(unobserved_bigrams$frequency)

probs_bigram_unobserved <- unobserved_bigrams %>%
  arrange(desc(frequency)) %>%
  mutate(prob = alpha_bigram * (frequency / nUnobservedBigrams)) %>%
  select(end = feature, prob)

# D. unobserved trigrams

sum_of_unobserved_bigram_probs <- sum(probs_bigram_unobserved$prob)
probs_trigram_unobserved <- probs_bigram_unobserved %>%
  arrange(desc(prob)) %>%
  mutate(prob_updated = alpha_trigram * (prob / sum_of_unobserved_bigram_probs)) %>%
  select(end, prob = prob_updated) %>%
  arrange(desc(prob))

# E. determine if inputted words are observed
# (in this case, observed_bigrams will have nrows > 0)
# (if inputted words are observed, use most likely probability based on observed)
# (if inputted words are not observed, use most likely probability based on KBO)
if (nrow(observed_bigrams) > 0) {
  probs_trigram$end[1]
} else {
  probs_trigram_unobserved$end[1]
}
