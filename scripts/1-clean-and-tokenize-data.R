# this script cleans and tokenizes text data
# in preparation for n-gram model creation
# written by shelby bachman, 2019
# last updated 18.10.2019

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

###### FUNCTIONS

# this function cleans text data in each corpus
# using tm package for this section
# my approach is based on my reading of this tutorial: 
# https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
clean_text_data <- function(this_corpus) {
  
  # 1. change all text to lowercase
  this_corpus_cleaned <- tm_map(this_corpus, content_transformer(tolower))
  
  # 2. remove numbers from corpus
  this_corpus_cleaned <- tm_map(this_corpus_cleaned, removeNumbers)
  
  # 3. replace some punctuation with spaces, including :, -, /, \, |
  # use the tm package to create a "content_transformer" function
  # this function takes a function as input, which specifies what transformation is done
  # in this particular case, gsub is used to detect punctuation of interest & replace with a space
  convert_to_space <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ', x))})
  this_corpus_cleaned <- tm_map(this_corpus_cleaned, convert_to_space, '/')
  this_corpus_cleaned <- tm_map(this_corpus_cleaned, convert_to_space, '@')
  this_corpus_cleaned <- tm_map(this_corpus_cleaned, convert_to_space, '#')
  
  # 4. remove punctuation
  this_corpus_cleaned <- tm_map(this_corpus_cleaned, removePunctuation)
  
  # 5. strip extra whitespace
  this_corpus_cleaned <- tm_map(this_corpus_cleaned, stripWhitespace)
  
  # 6. remove common words in english (for list, see: http://en.wikipedia.org/wiki/Stop_words)
  # this step takes longer than the others
  # skipping for now (done in quanteda during dfm creation)
  #this_corpus_cleaned <- tm_map(this_corpus_cleaned, removeWords, stopwords('english'))
  
  return(this_corpus_cleaned)
  
}

# this function tokenizes data in this corpus
# using quanteda package for this section
# my approach is based on my reading of this tutorial:
# https://tutorials.quanteda.io
tokenize_text_data <- function(this_corpus_cleaned) {
  
  # convert tm corpus to quanteda corpus 
  this_corpus_cleaned_q <- corpus(this_corpus_cleaned)
  
  # segment corpus into tokens by word boundaries
  this_corpus_tokens <- tokens(this_corpus_cleaned_q,
                               remove_punct = TRUE,
                               remove_symbols = TRUE,
                               remove_twitter = TRUE,
                               remove_url = TRUE)
  
  
  # remove english stop-words
  #this_corpus_tokens <- tokens_select(this_corpus_tokens, 
  #pattern = stopwords('en'), 
  #selection = 'remove')
  
  # generate n-grams of lengths 2 and 3 from tokenized object
  #this_corpus_ngrams <- tokens_ngrams(this_corpus_tokens_nostop, 
  #n = 2:3)
  
  # have a look the created n-grams
  #head(this_corpus_ngrams[[1]], 50)
  
  return(this_corpus_tokens)
  
}

###### loop through subsetted data and iteratively load and clean each file
# after the cleaning and tokenization steps, save the file
textFiles <- 1:2
for (ii in 1:length(textFiles)) {
  
  filename_toRead <- paste(path('data/data_subset/subset_'), textFiles[ii], '.txt', sep = '')
  filename_full <- str_split(filename_toRead, '/')[[1]][9]
  filename_prefix <- str_split(filename_full, '.txt')[[1]][1]
  
  # A. load the text file as a corpus
  this_corpus <- Corpus(DirSource(path('data/data_subset'), pattern = filename_full))
  
  # B. clean the corpus using tm (~)
  this_corpus_cleaned <- clean_text_data(this_corpus)
  
  rm(this_corpus)
  
  # C. tokenize the corpus using quanteda
  this_corpus_tokens <- tokenize_text_data(this_corpus_cleaned)
  
  # write the tokenized data to file
  filename_tokenized <- paste(path('data/data_tokenized/'), filename_prefix, '_tokenized.RData', sep = '')
  save(this_corpus_tokens, file = filename_tokenized)
  
}
