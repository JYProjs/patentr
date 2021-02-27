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
count_xml1 <- function(filename) {
  search_term <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  counter <- 0
  
  # read file line-by-line
  con <- file(filename, "r")
  while (TRUE) {
    curr_line <- readLines(con, n = 1)
    if (length(curr_line) == 0) break
    
    # check if line contains new patent
    if (grepl(x = curr_line, pattern = search_term, fixed = TRUE)) {
      counter <- counter + 1
    }
  }
  close(con)
  
  # return
  return(counter)
}

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
