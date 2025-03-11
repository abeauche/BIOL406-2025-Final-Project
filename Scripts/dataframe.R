#This Script is for cleaning and exporting dataframe

# === Import packages and dataframes

library(tidyverse)

df <- read.csv("./data_raw/Data_Collection - Sheet1.csv",stringsAsFactors = TRUE)

str(df)

view(df)

#Clean out NAs and notes

df <- df %>% 
  select(-notes)

#Merge with other data if needed?

#Mutate dataframes needed.