#Author: R Groenewegen
#Date: 2024-11-13

# Load packages ---------
library(tidyverse)

# ICA Data ---------

## Load data ---------
ica_raw <- read.csv(
  "./data_raw/ICA-Historical-Normalised-Catastrophe-September-2024.csv"
  )

## Initial tidy of data frame ---------

# remove NA columns, remove excess spaces, change blanks, hyphens to real NA
ica <- ica_raw %>% 
  # remove excess spaces
  mutate(across(, str_trim)) %>% 
  # make blanks/hyphens NA
  mutate(across(where(is.character), ~if_else(. %in% c("-", ""), NA, .))) %>% 
  # remove non-data rows
  filter(rowSums(!is.na(.)) > 1) %>%
  # remove non-data columns
  select_if(~ !all(is.na(.))) %>%
  # first row is column names
  set_names(str_replace_all(.[1,], "\\s", "_")) %>%
  slice(-1) #remove first row

## Tidy data ---------

# remove $ (add to column name), convert to numeric  
tmp <- ica %>%
  rename_with(~paste0(., "_($)", recycle0 = TRUE), contains("Value")) %>%
  mutate(across(everything(), ~str_replace(., "\\$", ""))) %>%
  


## Fix variable classes : the fun bit :| ---------

str(ica)

# split, strip, unlist states

states <- ica$` State `
tmp <- unlist(str_split(states[1], " "))
tmp[!(tmp %in% '&')]

tmp <- ica %>%
  mutate(State = ica$` State `)

# unlist postcodes



# dates

