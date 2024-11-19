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
  mutate(across(everything(), str_trim)) %>% 
  # make blanks/hyphens NA
  mutate(across(where(is.character), ~if_else(. %in% c("$-", "-", ""), NA, .))) %>% 
  # remove non-data rows
  filter(rowSums(!is.na(.)) > 1) %>%
  # remove non-data columns
  select_if(~ !all(is.na(.))) %>%
  # first row is column names
  set_names(str_replace_all(.[1,], "\\s", "_")) %>%
  slice(-1) %>% #remove first row 
  rowid_to_column() # unique identifier to each row to keep track

## Tidy data ---------

# unlist states, postcodes

unique(ica$State) # what are we dealing with?

ica <- ica %>%
  # remove special characters in state, make sure all upper case
  mutate(State = gsub("[^[:alpha:]]+", " ", State) %>% toupper) %>%
  # mutate(State = str_replace_all(State, "\\/|,|&|-", " ") %>% toupper) %>%
  # one row per state
  mutate(State = str_split(State, "\\s+")) %>%
  unnest(State)

unique(ica$State) # check 

# split at commas then work on reduced set of issues 

ica <- ica %>%
  # split at commas
  mutate(Postcode = strsplit(Postcode, ",")) %>%
  # one row per postcode
  unnest(Postcode) %>%
  # remove spaces
  mutate(Postcode = str_trim(Postcode, "both")) %>%
  # make numeric variable
  # NAs are deliberately introduced by coercion, thus suppress
  mutate(Postcode_numeric = suppressWarnings(as.numeric(Postcode))) 

## NEED TO DO WHILST KEEPING STATE FOR JOINING LATER##

# expand hyphenated groups
complete_postcodes <- ica %>%
  select(rowid, State, Postcode) %>%
  mutate(Postcode = strsplit(Postcode, ",")) %>%
  unnest(Postcode) %>%
  mutate(Postcode = str_trim(Postcode, "both")) %>%
  filter(str_detect(Postcode, "-")) %>% # cases with hyphens
  mutate(Start = as.numeric(substr(Postcode, 1, 4)), 
         End = as.numeric(substr(Postcode, nchar(Postcode) - 3, 
                                 nchar(Postcode)))) %>%
  pivot_longer(c(Start, End), values_to = "Postcode_numeric") %>%
  group_by(rowid, State, Postcode) %>%
  #fill(value, .direction = "down")
  complete(Postcode_numeric = full_seq(Postcode_numeric,1)) %>%
  select(-name)

# clean postcodes with non-numeric characters
clean_postcodes <- ica %>%
  select(rowid, State, Postcode) %>%
  mutate(Postcode = strsplit(Postcode, ",")) %>%
  unnest(Postcode) %>%
  mutate(Postcode = str_trim(Postcode, "both")) %>%
  # cases with non-digits, but not hyphens
  filter(str_detect(Postcode, "\\D+") & !str_detect(Postcode, "-")) %>%
  # remove non-digits, spaces, insert comma after 4 digits, split
  mutate(Postcode = gsub("\\D+", "", Postcode)) %>% 
  # split after 4 digits
  mutate(Postcode = strsplit(Postcode, "(?<=[\\d]{4})", perl = TRUE)) %>%
  unnest(Postcode, keep_empty = TRUE) %>%
  mutate(Postcode_numeric = as.numeric(Postcode))

ica <- left_join(ica, full_join(complete_postcodes, clean_postcodes)) %>%
  mutate(dummy = paste(State, "_", Postcode_numeric, sep = ""))

## NEED TO MATCH POSTCODES TO STATES AFTER UNNESTING ##

# data from https://www.matthewproctor.com/australian_postcodes 
# includes lga codes, ABS area codes, some spatial data

# filter by postcodes found in ica data then left/right join to ica data ???
# make dummy of STATE_POSTCODE in both then filter `ica` %in% `postcodes`

# get ICA postcodes (need NA rows to use in join so as not to lose rows)
postcodes_ica <- ica %>%
  select(State, Postcode_numeric) %>%
  unique()

postcodes_raw <- read.csv("./data_raw/australian_postcodes.csv")
colnames(postcodes)

# postcodes raw might need some cleaning - more than one lga per postcode??

# reduce to state, postcode, lga, SA* - for now, can change later 
postcodes <- postcodes_raw %>%
  select(state, postcode, lgacode, contains("SA", ignore.case = FALSE)) %>%
  rename(State = state, Postcode_numeric = postcode) %>%
  filter(Postcode_numeric %in% postcodes_ica$Postcode_numeric) %>%
  unique() %>%
  mutate(dummy = paste(State, "_", Postcode_numeric, sep = ""))

tmp <- ica %>%
  filter(dummy %in% postcodes$dummy)

tmp1 <- tmp %>%
  left_join(postcodes, by = "dummy") %>%
  select(-dummy)


# remove $ (add to column name), make every column with only digits numeric  
tmp <- ica %>%
  rename_with(~paste0(., "_($)", recycle0 = TRUE), contains("Value")) %>%
  mutate(across(everything(), ~str_remove(., "\\$"))) %>%
  mutate(across(contains("$"), ~str_remove_all(., ",") %>% as.numeric(.)))
  
tmp1 <- as.numeric(tmp$`Original_Loss_Value_($)`)

sum(is.na(tmp$`Original_Loss_Value_($)`)) 
sum(is.na(tmp1))
tmp2 <- data.frame(cbind(tmp$`Original_Loss_Value_($)`, tmp1))
tmp2 <- tmp2 %>%
  mutate(mismatch = as.numeric(tmp2$V1) == tmp2$tmp1)

## Fix variable classes : the fun bit :| ---------



# dates



