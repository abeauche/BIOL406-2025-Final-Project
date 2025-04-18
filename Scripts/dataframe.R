#This Script is for cleaning and exporting dataframe

# === Import packages and dataframes

library(tidyverse)

df <- read.csv("./data_raw/Data_Collection - Sheet1.csv",stringsAsFactors = TRUE)

str(df)

#Clean out NAs and notes

df <- df %>% 
  select(-notes)

#Merge with other data if needed?

#Mutate dataframes needed.

#Export dataframe

write_csv(df,"./data_cleaned/cleaned_dataframe.csv")

