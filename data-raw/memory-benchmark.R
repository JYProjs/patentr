# setup
library(patentr)
library(tidyverse)

# sample dataset
# - 10 TXT : first week of 1976 thru 1985
# - 10 XML1: first 5 weeks of 2003, first 5 weeks of 2004
# - 10 XML2: first week of 2005 thru 2014
sample_data <- data.frame(
  Week = c(rep(1, 10), rep(1:5, 2), rep(1, 10)),
  Year = c(1976:1985, rep(2003, 5), rep(2004, 5), 2005:2014)
  
  # manually collected from USPTO website (after download and extraction)
  
)

sample_data1 <- sample_data %>%
  slice(1:10) %>%
  mutate(Size_MB = map2_dbl(Year, Week, .f = function(year, week) {
    # download & save file
    get_bulk_patent_data(year = year, week = week, output_file = "temp.csv")
    
    # collect file size
    file_size <- file.size("temp.csv")
    
    # delete file
    file.remove("temp.csv")
    
    # return file size (in MB)
    return(file_size / 1024 ^ 2)
  }))
write_csv(sample_data1, "sample_data1.csv")

sample_data2 <- sample_data %>%
  slice(11:20) %>%
  mutate(Size_MB = map2_dbl(Year, Week, .f = function(year, week) {
    # download & save file
    get_bulk_patent_data(year = year, week = week, output_file = "temp.csv")
    
    # collect file size
    file_size <- file.size("temp.csv")
    
    # delete file
    file.remove("temp.csv")
    
    # return file size (in MB)
    return(file_size / 1024 ^ 2)
  }))

sample_data3 <- sample_data %>%
  slice(11:20) %>%
  mutate(Size_MB = map2_dbl(Year, Week, .f = function(year, week) {
    # download & save file
    get_bulk_patent_data(year = year, week = week, output_file = "temp.csv")
    
    # collect file size
    file_size <- file.size("temp.csv")
    
    # delete file
    file.remove("temp.csv")
    
    # return file size (in MB)
    return(file_size / 1024 ^ 2)
  }))
