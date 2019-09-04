# this script loads and samples the raw data
# for shelby's data science capstone project
# written by shelby bachman, 2019
# last updated 04.09.2019

# this script loads raw data files data/data_raw/en_US/*
# for each raw data file, it outputs 3 .csv files 
# each containing a random subset of 0.1% of lines from each file

###### setup
# clear workspace
rm(list = ls())

# load packages
library(rprojroot)
library(data.table)

# source files within this project
path <- function(x) find_root_file(x, criterion = has_file('data-science-capstone.Rproj'))

###### determine number of lines in data files

# en_US.twitter.txt
n_lines <- 0
filecon <- file(path('data/data_raw/en_US/en_US.twitter.txt'), open='r')
( while((lines_read <- length(readLines(filecon,20000))) > 0 )
  n_lines <- n_lines + lines_read )
close(filecon)
nLines_twitter <- n_lines
rm(n_lines, filecon, lines_read)

# en_US.news.txt
n_lines <- 0
filecon <- file(path('data/data_raw/en_US/en_US.news.txt'), open='r')
( while((lines_read <- length(readLines(filecon,20000))) > 0 )
  n_lines <- n_lines + lines_read )
close(filecon)
nLines_news <- n_lines
rm(n_lines, filecon, lines_read)

# en_US.blogs.txt
n_lines <- 0
filecon <- file(path('data/data_raw/en_US/en_US.blogs.txt'), open='r')
( while((lines_read <- length(readLines(filecon,20000))) > 0 )
  n_lines <- n_lines + lines_read )
close(filecon)
nLines_blogs <- n_lines
rm(n_lines, filecon, lines_read)

###### create random subsets of the lines in each data file
# (for each file, creating 3 random subsets)

# set number of subset files to create
nFiles <- 3

# initialize random number generator
set.seed(18)

# set probability of reading each line in the file
prob = 0.001

### en_US.twitter.txt

# create a subset file in each iteration of the loop
# during each iteration, open the file connect, 
# read random lines, save a file with the read lines, 
# and close the file connection
for (ii in 1:nFiles) {
  
  # decide whether or not to read each line
  # 0.5% of the time, read the line
  toss_twitter <- rbinom(n = nLines_twitter, size = 1, prob = prob)
  
  # open file connection
  filecon <- file(path('data/data_raw/en_US/en_US.twitter.txt'), 'r')
  
  # initialize data frame to store lines that have been read
  data_twitter <- data.frame()
  
  # loop through lines in the file and read random selection
  for (jj in 1:nLines_twitter) {
    if (toss_twitter[jj] == 1) {
      data_twitter <- rbind(data_twitter, 
                            as.data.frame(readLines(filecon, 1)))
    }
  }
  colnames(data_twitter) <- 'text'
  
  # set name of file where data will be saved
  output_file <- paste(path('data/data_subset/'), 'en_US_twitter_subset_', ii, '.csv', sep = '')
  
  # write data to file
  write.csv(data_twitter, 
            file = output_file,
            quote = FALSE, row.names = FALSE)
  
  # close file connection
  close(filecon)
  
}

### en_US.news.txt

# create a subset file in each iteration of the loop
# during each iteration, open the file connect, 
# read random lines, save a file with the read lines, 
# and close the file connection
for (ii in 1:nFiles) {
  
  # decide whether or not to read each line
  # 0.5% of the time, read the line
  toss_news <- rbinom(n = nLines_news, size = 1, prob = prob)
  
  # open file connection
  filecon <- file(path('data/data_raw/en_US/en_US.news.txt'), 'r')
  
  # initialize data frame to store lines that have been read
  data_news <- data.frame()
  
  # loop through lines in the file and read random selection
  for (jj in 1:nLines_news) {
    if (toss_news[jj] == 1) {
      data_news <- rbind(data_news, 
                            as.data.frame(readLines(filecon, 1)))
    }
  }
  colnames(data_news) <- 'text'
  
  # set name of file where data will be saved
  output_file <- paste(path('data/data_subset/'), 'en_US_news_subset_', ii, '.csv', sep = '')
  
  # write data to file
  write.csv(data_news, 
            file = output_file,
            quote = FALSE, row.names = FALSE)
  
  # close file connection
  close(filecon)
  
}

### en_US.blogs.txt

# create a subset file in each iteration of the loop
# during each iteration, open the file connect, 
# read random lines, save a file with the read lines, 
# and close the file connection
for (ii in 1:nFiles) {
  
  # decide whether or not to read each line
  # 0.5% of the time, read the line
  toss_blogs <- rbinom(n = nLines_blogs, size = 1, prob = prob)
  
  # open file connection
  filecon <- file(path('data/data_raw/en_US/en_US.blogs.txt'), 'r')
  
  # initialize data frame to store lines that have been read
  data_blogs <- data.frame()
  
  # loop through lines in the file and read random selection
  for (jj in 1:nLines_blogs) {
    if (toss_blogs[jj] == 1) {
      data_blogs <- rbind(data_blogs, 
                         as.data.frame(readLines(filecon, 1)))
    }
  }
  colnames(data_blogs) <- 'text'
  
  # set name of file where data will be saved
  output_file <- paste(path('data/data_subset/'), 'en_US_blogs_subset_', ii, '.csv', sep = '')
  
  # write data to file
  write.csv(data_blogs, 
            file = output_file,
            quote = FALSE, row.names = FALSE)
  
  # close file connection
  close(filecon)
  
}






