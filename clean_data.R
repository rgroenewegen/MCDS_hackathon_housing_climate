#Author: R Groenewegen
#Date: 2024-11-13

# Load packages ---------
library(tidyverse)

# ICA Data ---------

## Load data ---------
ica <- read.csv(
  "./data_raw/ICA-Historical-Normalised-Catastrophe-September-2024.csv"
  )

## Initial tidy ---------
ica[,1:6]
# remove heading lines
ica <- ica[-c(1:8),]
ica[,1:6] #check

head(ica[,(ncol(ica)-6):ncol(ica)])
tail(ica[,1:6])
# remove NA columns
ica <- ica %>% select_if(~ !all(is.na(.)))
colnames(ica) <- ica[1,]
colnames(ica)
ica <- ica[-1,]

## Fix variable classes : the fun bit :| ---------

# strip, unlist states
# unlist postcodes

# dates
str(ica)
