#### setup global environment for word prediction app
#### written by shelby bachman, 2020
#### last updated 3 may 2020

# load libraries
library(shiny)
library(shinythemes)
library(rprojroot)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# load frequency data
load('./data/freq_unigram_short.RData')
load('./data/freq_bigram_short.RData')
load('./data/freq_trigram_short.RData')
load('./data/freq_quadgram_short.RData')
