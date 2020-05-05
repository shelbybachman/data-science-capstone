# this script tests the accuracy of the prediction model
# shelby bachman, 2020
# last updated 5 may 2020

# clear workspace
rm(list = ls())

# load libraries
library(rprojroot)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# path to this project
path <- function(x) find_root_file(x, criterion = has_file('data-science-capstone.Rproj'))

# function to predict next word
source(path('scripts/predict_next_word.R'))

# number of samples
n_samples <- 25

################# TRAINING SET 1 (all n-grams)
# training set: 6 data subsets (20% of all data) used to train model
# test set: 1 data subset (not in training set), thus 16.7% of training set size

# load training dataset
load(path('data/data_dfm/freq_unigram.RData'))
load(path('data/data_dfm/freq_bigram.RData'))
load(path('data/data_dfm/freq_trigram.RData'))
load(path('data/data_dfm/freq_quadgram.RData'))

# load test set & keep only frequent n-grams
load(path('data/data_dfm/freq_trigram_test.RData'))
test_freq_trigram <- test_freq_trigram %>%
  filter(frequency > 50)
load(path('data/data_dfm/freq_quadgram_test.RData'))
test_freq_quadgram <- test_freq_quadgram %>%
  filter(frequency > 50)

#### test model accuracy for frequent n-grams:
# how well does training set predict next word 
# of randomly selected n-gram from the test set?
evaluate_accuracy <- function(x, y) {
  results <- predict_next_word(x, n_to_return = 50, return_df = TRUE)
  results$rank <- 1:nrow(results)
  if (y %in% results$end) {
    if (which(results$end == y) == which(results$rank == 1)) {
      accuracy = 1
    } else {
      accuracy = (50 - (results$rank[which(results$end == y)]) + 1) / 50
    }
  } else {
    accuracy = 0
  }
  return(accuracy)
}


## test of trigram accuracy
inds <- sample(1:nrow(test_freq_trigram), n_samples)
freq_val_trigram <- test_freq_trigram$frequency[inds]
accuracy_trigram <- NULL
for (ii in 1:length(inds)) {
  accuracy_trigram[ii] <- evaluate_accuracy(test_freq_trigram$start[inds[ii]], test_freq_trigram$end[inds[ii]])
}

## test of quadgram accuracy
inds <- sample(1:nrow(test_freq_quadgram), n_samples)
freq_val_quadgram <- test_freq_quadgram$frequency[inds]
accuracy_quadgram <- NULL
for (ii in 1:length(inds)) {
  accuracy_quadgram[ii] <- evaluate_accuracy(test_freq_quadgram$start[inds[ii]], test_freq_quadgram$end[inds[ii]])
}

#### test model accuracy: less frequent n-grams

# load test set & keep only less frequent n-grams
load(path('data/data_dfm/freq_trigram_test.RData'))
test_freq_trigram <- test_freq_trigram %>%
  filter(frequency < 25 & frequency > 10)
load(path('data/data_dfm/freq_quadgram_test.RData'))
test_freq_quadgram <- test_freq_quadgram %>%
  filter(frequency < 25 & frequency > 10)

#### test of trigram accuracy
inds <- sample(1:nrow(test_freq_trigram), n_samples)
freq_val_trigram2 <- test_freq_trigram$frequency[inds]
accuracy_trigram_lowFreq <- NULL
for (ii in 1:length(inds)) {
  accuracy_trigram_lowFreq[ii] <- evaluate_accuracy(test_freq_trigram$start[inds[ii]], test_freq_trigram$end[inds[ii]])
}

#### test of quadgram accuracy
inds <- sample(1:nrow(test_freq_quadgram), n_samples)
freq_val_quadgram2 <- test_freq_quadgram$frequency[inds]
accuracy_quadgram_lowFreq <- NULL
for (ii in 1:length(inds)) {
  accuracy_quadgram_lowFreq[ii] <- evaluate_accuracy(test_freq_quadgram$start[inds[ii]], test_freq_quadgram$end[inds[ii]])
}

accuracy_data_set1 <- as.data.frame(rbind(
  data.frame(accuracy = accuracy_quadgram, ngram = rep('4-gram', n_samples), 
             freq_level = rep('high frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_quadgram),
  data.frame(accuracy = accuracy_trigram, ngram = rep('3-gram', n_samples), 
             freq_level = rep('high frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_trigram),
  data.frame(accuracy = accuracy_quadgram_lowFreq, ngram = rep('4-gram', n_samples), 
             freq_level = rep('low frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_quadgram2),
  data.frame(accuracy = accuracy_trigram_lowFreq, ngram = rep('3-gram', n_samples), 
             freq_level = rep('low frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_trigram2)
))

write.csv(accuracy_data_set1, path('data/data_accuracy/accuracy_set1.csv'), quote = FALSE, row.names = FALSE)
rm(this_freq_unigram, this_freq_bigram, this_freq_trigram, this_freq_quadgram)

################# TRAINING SET 2 (only n-grams with freq > 2)
# training set: 6 data subsets (20% of all data) used to train model
# test set: 1 data subset (not in training set), thus 16.7% of training set size

# load training dataset
load(path('app/data/freq_unigram_short.RData'))
load(path('app/data/freq_bigram_short.RData'))
load(path('app/data/freq_trigram_short.RData'))
load(path('app/data//freq_quadgram_short.RData'))

# load test set & keep only frequent n-grams
load(path('data/data_dfm/freq_trigram_test.RData'))
test_freq_trigram <- test_freq_trigram %>%
  filter(frequency > 50)
load(path('data/data_dfm/freq_quadgram_test.RData'))
test_freq_quadgram <- test_freq_quadgram %>%
  filter(frequency > 50)

#### test model accuracy for frequent n-grams:
# how well does training set predict next word 
# of randomly selected n-gram from the test set?
evaluate_accuracy <- function(x, y) {
  results <- predict_next_word(x, n_to_return = 50, return_df = TRUE)
  results$rank <- 1:nrow(results)
  if (y %in% results$end) {
    if (which(results$end == y) == which(results$rank == 1)) {
      accuracy = 1
    } else {
      accuracy = (50 - (results$rank[which(results$end == y)]) + 1) / 50
    }
  } else {
    accuracy = 0
  }
  return(accuracy)
}

## test of trigram accuracy
inds <- sample(1:nrow(test_freq_trigram), n_samples)
freq_val_trigram <- test_freq_trigram$frequency[inds]
accuracy_trigram <- NULL
for (ii in 1:length(inds)) {
  accuracy_trigram[ii] <- evaluate_accuracy(test_freq_trigram$start[inds[ii]], test_freq_trigram$end[inds[ii]])
}

## test of quadgram accuracy
inds <- sample(1:nrow(test_freq_quadgram), n_samples)
freq_val_quadgram <- test_freq_quadgram$frequency[inds]
accuracy_quadgram <- NULL
for (ii in 1:length(inds)) {
  accuracy_quadgram[ii] <- evaluate_accuracy(test_freq_quadgram$start[inds[ii]], test_freq_quadgram$end[inds[ii]])
}

#### test model accuracy: less frequent n-grams

# load test set & keep only less frequent n-grams
load(path('data/data_dfm/freq_trigram_test.RData'))
test_freq_trigram <- test_freq_trigram %>%
  filter(frequency < 25 & frequency > 10)
load(path('data/data_dfm/freq_quadgram_test.RData'))
test_freq_quadgram <- test_freq_quadgram %>%
  filter(frequency < 25 & frequency > 10)

#### test of trigram accuracy
inds <- sample(1:nrow(test_freq_trigram), n_samples)
freq_val_trigram2 <- test_freq_trigram$frequency[inds]
accuracy_trigram_lowFreq <- NULL
for (ii in 1:length(inds)) {
  accuracy_trigram_lowFreq[ii] <- evaluate_accuracy(test_freq_trigram$start[inds[ii]], test_freq_trigram$end[inds[ii]])
}

#### test of quadgram accuracy
inds <- sample(1:nrow(test_freq_quadgram), n_samples)
freq_val_quadgram2 <- test_freq_quadgram$frequency[inds]
accuracy_quadgram_lowFreq <- NULL
for (ii in 1:length(inds)) {
  accuracy_quadgram_lowFreq[ii] <- evaluate_accuracy(test_freq_quadgram$start[inds[ii]], test_freq_quadgram$end[inds[ii]])
}

accuracy_data_set2 <- as.data.frame(rbind(
  data.frame(accuracy = accuracy_quadgram, ngram = rep('4-gram', n_samples), 
             freq_level = rep('high frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_quadgram),
  data.frame(accuracy = accuracy_trigram, ngram = rep('3-gram', n_samples), 
             freq_level = rep('high frequency', n_samples), indx = 1:n_samples,
             freq_test = freq_val_trigram),
  data.frame(accuracy = accuracy_quadgram_lowFreq, ngram = rep('4-gram', n_samples), 
             freq_level = rep('low frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_quadgram2),
  data.frame(accuracy = accuracy_trigram_lowFreq, ngram = rep('3-gram', n_samples), 
             freq_level = rep('low frequency', n_samples), indx = 1:n_samples, 
             freq_test = freq_val_trigram2)
))

write.csv(accuracy_data_set2, path('data/data_accuracy/accuracy_set1.csv'), quote = FALSE, row.names = FALSE)
rm(this_freq_unigram, this_freq_bigram, this_freq_trigram, this_freq_quadgram)

################# PLOTS 

accuracy_data_set1$ngram <- factor(accuracy_data_set1$ngram, levels = c('3-gram', '4-gram'))
accuracy_data_set2$ngram <- factor(accuracy_data_set2$ngram, levels = c('3-gram', '4-gram'))

p_set1 <- ggplot(data = accuracy_data_set1 %>% arrange(ngram, freq_test), aes(x = freq_test, y = accuracy, colour = factor(ngram), group = factor(ngram))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian(xlim = c(50, 300), ylim = c(0.7, 1)) +
  labs(x = 'Frequency of n-gram in test set', y = 'Accuracy', colour = '',
       title = 'Training set:\nAll n-grams') +
  theme_pubr()

p_set2 <- ggplot(data = accuracy_data_set2 %>% arrange(ngram, freq_test), aes(x = freq_test, y = accuracy, colour = factor(ngram), group = factor(ngram))) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian(xlim = c(50, 300), ylim = c(0.7, 1)) +
  labs(x = 'Frequency of n-gram in test set', y = 'Accuracy', colour = '',
       title = 'Training set for app:\nn-grams with frequency > 2') +
  theme_pubr()

p <- ggarrange(p_set1, p_set2, nrow = 1, common.legend = TRUE)
ggsave(path('reports/model_accuracy.png'), p, dpi = 300, width = 7, height = 4)
