# this script demos the final prediction model
# for shelby's data science capstone project
# shelby bachman, 2020
# last updated 5 may 2020

# NOTE: this script uses full n-gram frequency dataframes
# whereas the app uses only n-grams with frequency > 2

###### setup
# clear workspace
rm(list = ls())

# load packages
library(rprojroot)
library(dplyr)
library(stringr)

# source files within this project
path <- function(x) find_root_file(x, criterion = has_file('data-science-capstone.Rproj'))

# load frequency tables
load(path('data/data_dfm/freq_unigram.RData'))
load(path('data/data_dfm/freq_bigram.RData'))
load(path('data/data_dfm/freq_trigram.RData'))
load(path('data/data_dfm/freq_quadgram.RData'))

# source function to predict next word
source(path('app/functions/predict_next_word.R'))

# demo prediction model
predict_next_word('went for a', n_to_return = 50, return_df = TRUE)

predict_next_word('are you going to the', n_to_return = 5, return_df = TRUE)

predict_next_word("you're not going to believe", n_to_return = 10, return_df = FALSE)

predict_next_word('i am #excited for the', n_to_return = 5, return_df = TRUE)


### test version of training set for app
load(path('app/data/freq_unigram_short.RData'))
load(path('app/data/freq_bigram_short.RData'))
load(path('app/data/freq_trigram_short.RData'))
load(path('app/data/freq_quadgram_short.RData'))

predict_next_word('went for a', n_to_return = 15, return_df = FALSE)

data <- predict_next_word('new york', n_to_return = 25, return_df = TRUE)

# calculate relative probabilities
sum_probs <- sum(data$prob)
data <- data %>%
    mutate(prob_adj = prob / sum_probs)

# vertical barplot
p <- ggplot(data, aes(x = reorder(end, prob_adj), y = prob_adj)) +
  geom_bar(fill = '#738678', colour = '#222222', stat = "identity") +
  theme_pubr(base_size = 10) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        line = element_blank()) +
  labs(x = '', y = '') +
  coord_flip()

# save as sample output
ggsave(path('reports/sample_output.png'), p, dpi = 300, width = 3, height = 3)
