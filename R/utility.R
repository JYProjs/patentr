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
format_field_df <- function(vec) {
  paste0(vec, collapse = ";")
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
#' @import magrittr
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
