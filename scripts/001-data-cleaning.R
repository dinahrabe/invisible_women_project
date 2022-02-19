##### data cleaning ######

# Set working directory to source file location
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# UTF-8 lettering, enabling e.g. potential German 'Umlaute'
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# install necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,naniar)

# load dataset
raw_data <- read_csv("data/raw_data.csv")

# replace "character(0)" in tags and groups with NA
raw_data <- raw_data %>%
  replace_with_na(replace = list(tags = "character(0)")) %>%
  replace_with_na(replace = list(groups = "character(0)"))

##### clean the tags and groups column #####

#remove "c(" from start of string
raw_data$tags <- sub('^c[(]', '', raw_data$tags)
raw_data$groups <- sub('^c[(]', '', raw_data$groups)

#remove ")" from end of string
raw_data$tags <- gsub("[)]", "", raw_data$tags)
raw_data$groups <- gsub("[)]", "", raw_data$groups)

# create a new id column
clean_df <- raw_data[,2:6]
clean_df <- tibble::rowid_to_column(clean_df, "id")

## remove punctuation, lowercase

clean_df <- clean_df %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_replace_all(
      ., "[[:punct:]](?!\\w)", " ") #removes all punctuation that is not followed by a character
  )) %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_replace_all(
      ., '\\"|\\_|\\(', " ") #remove remaining " , _ and (
  )) %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_squish(.) #removes double whitespace
  )) %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_to_lower(.)
  ))

## transforming german umlaute

clean_df <- clean_df %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_replace_all(
      ., 'ä', "ae") #replace the ä with ae
  )) %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_replace_all(
      ., 'ö', "oe") #replace the ö with oe
  )) %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_replace_all(
      ., 'ü', "ue") #replace the ü with ue
  )) %>%
  mutate(across(
    .cols = title:groups, .fns = ~ str_replace_all(
      ., 'ß', "ss") #replace the ß with ss
  ))


#writes data as csv
write.csv(clean_df,paste0(getwd(),"/data/clean_data.csv"), row.names = FALSE)

