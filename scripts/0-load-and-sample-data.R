# this script loads and samples the raw data
# for shelby's data science capstone project
# written by shelby bachman, 2019
# last updated 18.10.2019

# this script loads raw data files data/data_raw/en_US/*
# for each raw data file, it outputs 3 .csv files 
# each containing a random subset of 0.1% of lines from each file

###### setup
# clear workspace
rm(list = ls())

# load packages
library(rprojroot)
library(R.utils)

# source files within this project
path <- function(x) find_root_file(x, criterion = has_file('data-science-capstone.Rproj'))

###### get file size & number of lines in each file

# en_US.twitter.txt
file_twitter <- path('data/data_raw/en_US/en_US.twitter.txt')
fileSize_twitter <- file.size(file_twitter) / (1000*1000) # get file size in mb
nLines_twitter <- as.numeric(countLines(file_twitter)) # get number of lines in file

# en_US.news.txt
file_news <- path('data/data_raw/en_US/en_US.news.txt')
fileSize_news <- file.size(file_news) / (1000*1000) # get file size in mb
nLines_news <- as.numeric(countLines(file_news)) # get number of lines in file

# en_US.blogs.txt
file_blogs <- path('data/data_raw/en_US/en_US.blogs.txt')
fileSize_blogs <- file.size(file_blogs) / (1000*1000) # get file size in mb
nLines_blogs <- as.numeric(countLines(file_blogs)) # get number of lines in file

# estimate size and number of lines if all files were combined
fileSize_total <- fileSize_twitter + fileSize_news + fileSize_blogs
nLines_total <- nLines_twitter + nLines_news + nLines_blogs

###### read in all files and combine into single corpus

filecon <- file(file_twitter, 'r')
text_twitter <- readLines(filecon, encoding = 'UTF-8', warn = FALSE)
close(filecon)

filecon <- file(file_news, 'r')
text_news <- readLines(filecon, encoding = 'UTF-8', warn = FALSE)
close(filecon)

filecon <- file(file_blogs, 'r')
text_blogs <- readLines(filecon, encoding = 'UTF-8', warn = FALSE)
close(filecon)

text_all <- c(text_twitter, text_news, text_blogs)
nLines_total <- length(text_all)
rm(text_twitter, text_news, text_blogs)

####### shuffle lines & subset into samples

# set number of lines for each subsample by rounding down
nSamples <-  30
nLinesPerSample <- floor(nLines_total / nSamples)

# randomly permute order of lines in text_all
text_all <- text_all[sample(nLines_total)]

for (ii in 1:nSamples) {
  
  # set indices of lines to read
  index_start <- nLinesPerSample*(ii-1) + 1
  index_end <- nLinesPerSample*ii
  indices <- index_start:index_end
  
  # subset lines and save
  text_subset <- text_all[indices] 
  
  # write subset to file
  filename_subset <- paste(path('data/data_subset/subset_'), ii, '.txt', sep = '')
  filecon <- file(filename_subset, 'w')
  writeLines(text_subset, filecon)
  close(filecon)
  rm(filename_subset, text_subset)
  
}

rm(text_all)

#####

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
#ls -alh (gets file size)

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