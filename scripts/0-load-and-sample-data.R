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

rm(data_twitter, data_news, data_blogs)

# regular expressions:

# ^ start of a line
# $ end of a line
# [Hh][Ii] matches HI, Hi, hI, hi
# [a-zA-z] matches any letter
# . refers to any character/symbol
# | one expression or another (can include more than two)
# ^[Gg]ood|[Bb]ad either good at start of line, or bad anywhere
# ^([Gg]ood|[Bb]ad) either good or bad at start of line
# ? indicated expression is optional
# \ escape the metacharacter (i.e. \. actually look for a period)
# * repeat any number of times, including none
# + at least one
# { } maximum and nimum number of matches of any expression
# see notes for max/min details


###### quiz 1
# find the number of lines where the words 'love' and 'hate'
# (all lowercase) occur in the twitter dataset
filecon = file(path('data/data_raw/en_US/en_US.twitter.txt'), 'r')
tally_love = 0
tally_hate = 0
while ( TRUE ) {
  line = readLines(filecon, 1)
  tally_love = tally_love + length(grep('[l][o][v][e]', line, value = TRUE))
  tally_hate = tally_hate + length(grep('[h][a][t][e]', line, value = TRUE))
  if ( length(line) == 0 ) {
    break
  }
}
close(filecon)
tally_love/tally_hate

# find the one tweet that has the word 'biostats'
filecon = file(path('data/data_raw/en_US/en_US.twitter.txt'), 'r')
while ( TRUE ) {
  line = readLines(filecon, 1)
  tally_love = tally_love + length(grep('[l][o][v][e]', line, value = TRUE))
  if (length(grep('biostats', line, value = TRUE)) == 1) {
    text <- grep('biostats', line, value = TRUE)
  }
  if ( length(line) == 0 ) {
    break
  }
}
text
close(filecon)

# determine how many tweets have the exact characters:
# A computer once beat me at chess, but it was no match for me at kickboxing
filecon = file(path('data/data_raw/en_US/en_US.twitter.txt'), 'r')
tally = 0
while ( TRUE ) {
  line = readLines(filecon, 1)
  tally = tally + length(grep('A computer once beat me at chess, but it was no match for me at kickboxing', line, value = TRUE))
  if ( length(line) == 0 ) {
    break
  }
}
close(filecon)
tally

####### command line options for all answers:
# get file size
ls -alh (gets file size)

#get number of lines
#awk '{print length}' filename |sort -nr|head -1

# find lines with 'love' and 'hate'
#love=$(grep "love" en_US.twitter.txt wc −l)
#hate=$(grep "hate" en_US.twitter.txt  wc−l)
#let m=love/hate
#echo $m

# find tweets with 'biostats'
# grep -i "biostat" en_US.twitter.txt

# find tweets with string of interest
# grep -x "A computer ..." en_US.twitter.txt wc -l
#grep "love" filename wc -l