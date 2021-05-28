# download file from USPTO website
# assumes year >= 1976 and <= current year
download_uspto <- function(year, week, destfile) {
  # get date given year and week
  # USPTO always uploads new files on Tuesdays
  ans_date <- get_date_tues(year = year, week = week)
  
  # pick correct function to pass to based on year
  if (year < 2002) {
    download_uspto_txt(ans_date, week, destfile)
  } else if (year < 2005) {
    download_uspto_xml1(ans_date, destfile)
  } else {
    download_uspto_xml2(ans_date, destfile)
  }
}

# download TXT file from USPTO website
#' @importFrom magrittr "%>%"
download_uspto_txt <- function(file_date, curr_week, destfile) {
  # base vars
  filename_uspto <- "pftapsYYYYMMDD_wkNN"
  txt_uspto_url <- "https://bulkdata.uspto.gov/data/patent/grant/redbook/fulltext/"
  dest_file <- "temp-output.zip"
  
  # figure out file names
  curr_year <- lubridate::year(file_date)
  curr_month <- lubridate::month(file_date)
  curr_day <- lubridate::day(file_date)
  curr_file <- filename_uspto %>%
    gsub(pattern = "YYYY", replacement = curr_year, fixed = TRUE) %>%
    gsub(pattern = "MM", replacement = int_with_len(curr_month, 2), fixed = TRUE) %>%
    gsub(pattern = "DD", replacement = int_with_len(curr_day, 2), fixed = TRUE) %>%
    gsub(pattern = "NN", replacement = int_with_len(curr_week, 2), fixed = TRUE)
  curr_url <- txt_uspto_url %>%
    paste0(curr_year, "/", curr_file, ".zip")
  curr_file <- paste0(curr_file, ".txt")
  
  # download appropriate zip from USPTO bulk website
  utils::download.file(url = curr_url,
                       destfile = dest_file)
  
  # uncompress
  utils::unzip(zipfile = dest_file)
  
  # delete zip
  file.remove(dest_file)
  
  # rename downloaded file to desired name
  file.rename(curr_file, destfile)
}

# download XML1 file from USPTO website
#' @importFrom magrittr "%>%"
download_uspto_xml1 <- function(file_date, destfile) {
  # base vars
  filename_uspto <- "pgYYMMDD"
  xml1_uspto_url <- "https://bulkdata.uspto.gov/data/patent/grant/redbook/fulltext/"
  dest_file <- "temp-output.zip"
  
  # figure out file names
  curr_month <- lubridate::month(file_date)
  curr_day <- lubridate::day(file_date)
  curr_year <- lubridate::year(file_date)
  curr_file <- filename_uspto %>%
    gsub(pattern = "YY", replacement = substr(curr_year, 3, 4), fixed = TRUE) %>%
    gsub(pattern = "MM", replacement = int_with_len(curr_month, 2), fixed = TRUE) %>%
    gsub(pattern = "DD", replacement = int_with_len(curr_day, 2), fixed = TRUE)
  curr_url <- xml1_uspto_url %>%
    paste0(curr_year, "/", curr_file, ".zip")
  extra_file1 <- paste0(curr_file, ".SGM")
  extra_file2 <- paste0(curr_file, ".sgm")
  curr_file1 <- paste0(curr_file, ".XML")
  curr_file2 <- paste0(curr_file, ".xml")
  
  # download appropriate zip from USPTO bulk website
  utils::download.file(url = curr_url,
                       destfile = dest_file)
  
  # uncompress
  utils::unzip(zipfile = dest_file)
  
  # if file doesn't exist, there's a problem
  if (!file.exists(curr_file1) &
      !file.exists(curr_file2)) {
    stop(paste("File", curr_file1, "and", curr_file2,
               "do not exist after unzipping"),
         call. = FALSE)
    # otherwise, assign the correct name to the curr_file var for later use
  } else if (file.exists(curr_file1)) {
    curr_file <- curr_file1
  } else if (file.exists(curr_file2)) {
    curr_file <- curr_file2
  } else {
    stop("Logically should never be able to reach this line")
  }
  
  # if extra file, then delete
  if (file.exists(extra_file1)) {
    file.remove(extra_file1)
  } else if (file.exists(extra_file2)) {
    file.remove(extra_file2)
  }
  
  # delete zip
  file.remove(dest_file)
  
  # rename downloaded file to desired name
  file.rename(from = curr_file, to = destfile)
}

# download XML2 file from USPTO website
#' @importFrom magrittr "%>%"
download_uspto_xml2 <- function(file_date, destfile) {
  # base vars
  filename_uspto <- "ipgYYMMDD"
  xml2_uspto_url <- "https://bulkdata.uspto.gov/data/patent/grant/redbook/fulltext/"
  dest_file <- "temp-output.zip"
  
  # figure out file names
  curr_month <- lubridate::month(file_date)
  curr_day <- lubridate::day(file_date)
  curr_year <- lubridate::year(file_date)
  curr_file <- filename_uspto %>%
    gsub(pattern = "YY", replacement = substr(curr_year, 3, 4), fixed = TRUE) %>%
    gsub(pattern = "MM", replacement = int_with_len(curr_month, 2), fixed = TRUE) %>%
    gsub(pattern = "DD", replacement = int_with_len(curr_day, 2), fixed = TRUE)
  curr_url <- xml2_uspto_url %>%
    paste0(curr_year, "/", curr_file, ".zip")
  curr_file1 <- paste0(curr_file, ".XML")
  curr_file2 <- paste0(curr_file, ".xml")
  
  # download appropriate zip from USPTO bulk website
  utils::download.file(url = curr_url,
                       destfile = dest_file)
  
  # uncompress
  utils::unzip(zipfile = dest_file)
  
  # if file doesn't exist, there's a problem
  if (!file.exists(curr_file1) &
      !file.exists(curr_file2)) {
    stop(paste("File", curr_file1, "and", curr_file2,
               "do not exist after unzipping"),
         call. = FALSE)
    # otherwise, assign the correct name to the curr_file var for later use
  } else if (file.exists(curr_file1)) {
    curr_file <- curr_file1
  } else if (file.exists(curr_file2)) {
    curr_file <- curr_file2
  } else {
    stop("Logically should never be able to reach this line")
  }
  
  # delete zip
  file.remove(dest_file)
  
  # rename downloaded file to desired name
  file.rename(from = curr_file, to = destfile)
}

# figure out date of Nth Tuesday in given year
# important b/c USPTO issues patents every Tues
#' @importFrom magrittr "%>%"
get_date_tues <- function(year, week) {
  # confirm valid params
  if (week < 1 | week > 53) {
    stop(paste("Invalid parameters: year =", year, "& week =", week),
         call. = FALSE)
  }

  # convert to integer (just in case)
  year <- as.integer(year)
  week <- as.integer(week)

  # find first Tues in the year (Tues = 3 in result of lubridate::wday)
  first_day <- paste0(year, "-01-01") %>%
    as.Date()
  while (lubridate::wday(first_day) != 3) {
    first_day <- first_day + 1
  }

  # add the number of weeks as required by user
  ans <- first_day + 7 * (week - 1)

  # return
  return(ans)
}

# count number of patents in XML1 formatted files
# count_xml1 <- function(filename) {
#   search_term <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
#   counter <- 0
#   
#   # read file line-by-line
#   con <- file(filename, "r")
#   while (TRUE) {
#     curr_line <- readLines(con, n = 1)
#     if (length(curr_line) == 0) break
#     
#     # check if line contains new patent
#     if (grepl(x = curr_line, pattern = search_term, fixed = TRUE)) {
#       counter <- counter + 1
#     }
#   }
#   close(con)
#   
#   # return
#   return(counter)
# }

# get integer with set number of digits (adds appropriate leading zeroes)
int_with_len <- function(int_val, len) {
  # doesn't work w/ negative numbers
  if (int_val < 0) {
    stop(paste("`int_val` cannot be negative; passed value =", int_val),
         call. = FALSE)
  }

  # convert to character, then add leading zeroes
  ans <- as.character(int_val)
  num_leading <- len - nchar(ans)
  if (num_leading < 0) {
    stop(paste("Provided length is too short for integer: int_val =", int_val,
               "& len =", len),
         call. = FALSE)
  }

  ans <- paste0(paste(rep("0", num_leading), collapse = ""), # leading zeroes
                ans) # actual integer value

  # return
  return(ans)
}

# format character vector for data frame
#' @import magrittr
format_field_df <- function(vec) {
  paste0(vec, collapse = ";") %>%
    # remove leading spaces
    gsub(pattern = "^ +", replacement = "") %>%
    # remove lagging spaces
    gsub(pattern = " +$", replacement = "")
}

# only alphanumeric characters
strip_nonalphanum <- function(char_vec) {
  gsub(x = char_vec,
       pattern = "[^0-9A-Za-z]",
       replacement = "")
}

# convert WKU to patent number (e.g. remove checksum digit)
# wku parameter should be a character vector
# source: https://www.uspto.gov/patents/apply/applying-online/patent-number
#' Get Patient Number from WKU
#'
#' Convert WKU identifier provided in bulk patent files to patent number
#' used in most sources. The References provided in bulk patent files are
#' also in patent number format, not in WKU format.
#'
#' @param wku character vector containing patent WKUs
#' @return character vector containing patent numbers
#' @export
#' @import magrittr
#' @importFrom rlang .data
#' @examples
#' # convert sample WKUs to patent number and print
#' sample_wku <- c("RE028671", "03930271")
#' print(wku_to_pno(sample_wku))
wku_to_pno <- function(wku) {
  vapply(X = wku,
         USE.NAMES = FALSE,
         FUN.VALUE = character(1),
         FUN = function(curr_wku) {
           # setup var that'll be returned
           curr_wku <- strip_nonalphanum(curr_wku)
           ans <- curr_wku
           
           # utility patents (6-8 numeric digits)
           if (grepl(x = curr_wku, pattern = "^[0-9]{6,}$")) {
             # remove leading zeroes
             ans <- gsub(x = curr_wku,
                         pattern = "^0+",
                         replacement = "")
             
           # reissue patents ("RE" followed by 6 digits, add leading zeroes)
           # plant patents ("PP" followed by 6 digits, add leading zeroes)
           # additions of improvements ("AI" followed by 6 digits, add leading zeroes)
           # above is likely how WKU was created from USPTO source, need to reverse
           } else if (grepl(x = curr_wku,
                            pattern = "^(RE)|(PP)|(AI)[0-9]{5,}")) {
             # store first two chars
             char_part <- substr(curr_wku, start = 1, stop = 2)
             
             # remove leading zeroes from numeric portion
             num_part <- curr_wku %>%
               substr(start = 3, stop = 100) %>%
               gsub(pattern = "^0+", replacement = "")
             
             # add back starting "RE"
             ans <- paste0(char_part, num_part)
             
           # design patents ("D" followed by 7 digits, add leading zeroes)
           # X patents ("X" followed by 7 digits, add leading zeroes)
           # H documents ("H" followed by 7 digits, add leading zeroes)
           # T documents ("T" followed by 7 digits, add leading zeroes)
           # above is likely how WKU was created from USPTO source, need to reverse
           } else if (grepl(x = curr_wku,
                            pattern = "^[DXHT][0-9]{4,}")) {
             # store first char
             char_part <- substr(curr_wku, start = 1, stop = 1)
             
             # remove leading zeroes from numeric portion
             num_part <- curr_wku %>%
               substr(start = 2, stop = 100) %>%
               gsub(pattern = "^0+", replacement = "")
             
             # add back starting char
             ans <- paste0(char_part, num_part)
           }
           # don't know what to do with this if no matches, return untouched
           
           # return
           return(ans)
         })
}

# remove final checksum digit from utility patents in TXT format
remove_txt_checksum <- function(wku) {
  end_substr <- nchar(wku) - 1
  substr(wku, start = 1, stop = end_substr)
}

# remove commas and quotes that cause issues with CSV file format
remove_csv_issues <- function(txt) {
  txt %>%
    gsub(pattern = '[",]', replacement = "")
}
