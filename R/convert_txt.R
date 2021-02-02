# internal function to convert txt bulk data
# input = file to convert (year & week)
# output = data frame
convert_txt_to_df <- function(year, week) {
  # download data - NEED TO DO

  # extract/uncompress downloaded data - NEED TO DO
  data_file <- ""

  # convert TXT file
  ans <- convert_txt_to_df(data_file)

  # return data frame
  return(ans)
}

# convert multiple TXT files
# internal function
# date_df: column1 = year; column2 = week
# output_file should be a CSV
# returns data frame of patent data of output_file == NULL; TRUE otherwise
# if:
#   - output_file = NULL is used to acquire data and stored into `df1`
#   - output_file = <filename> is used to acquire data and then read into `df2` with `read.csv`
#     (read.csv(<filename>, colClasses = rep("character", 8), na.strings = c("NA", "N/A", "")))
# then: all.equal(df1, df2) should return TRUE
convert_txt_to_df <- function(date_df, output_file = NULL) {
  # NEED TO COMPLETE: confirm dates and data frame format are valid
  if (!("Year" %in% colnames(date_df) & "Week" %in% colnames(date_df))) {
    stop(paste("date_df parameter must have `Year` and `Week` columns;",
               "available cols are:", colnames(date_df)),
         call. = FALSE)
  }

  # base vars
  filename_uspto <- "pftapsYYYYMMDD_wkNN"
  txt_uspto_url <- "https://bulkdata.uspto.gov/data/patent/grant/redbook/fulltext/"
  dest_file <- "temp-output.zip"

  # list to store data frames
  df_store <- vector(mode = "list", length = nrow(date_df))

  # create header for output file (if necessary)
  if (!is.null(output_file)) {
    cat("WKU,Title,App_Date,Issue_Date,Inventor,Assignee,ICL_Class,References\n",
        file = output_file)
  }

  # loop through date_df
  for (curr_row in seq_len(nrow(date_df))) {
    # curr vars for neater code
    curr_year <- date_df$Year[curr_row]
    curr_week <- date_df$Week[curr_row]

    # figure out date of given week in the year and adjust URL as necessary
    file_date <- get_date_tues(year = curr_year,
                               week = curr_week)
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

    # convert uncompressed file
    curr_df <- txt_to_df_r(input_file = curr_file, output_file = output_file)

    # delete uncompressed file
    file.remove(curr_file)

    # store data frame into list or output based on user preference
    if (is.null(output_file)) {
      df_store[[curr_row]] <- curr_df
    }
  }

  # combine all data frames in list
  ans <- TRUE
  if (is.null(output_file)) {
    ans <- data.table::rbindlist(df_store)
    attr(ans, ".internal.selfref") <- NULL  # remove attribute for equality between file read and direct df methods
  }

  # return
  return(ans)
}

# convert TXT file containing patent data to data frame
# NEED TO REMOVE SECOND PARAMETER DEFAULT VALUE - maybe preserve?
txt_to_df_r <- function(input_file, output_file = NULL) {
  # placeholder
  temp_output_file <- ifelse(is.null(output_file),
                             "temp-patent-package-output.csv",
                             output_file)
  append_bool <- ifelse(is.null(output_file), FALSE, TRUE)
  header_bool <- ifelse(is.null(output_file), TRUE, ifelse(file.exists(output_file), FALSE, TRUE))

  # convert TXT to CSV
  num_pat <- txt_to_df_cpp(input_file, temp_output_file, append_bool, header_bool)

  # read CSV as data frame and return
  ans <- TRUE
  if (is.null(output_file)) {
    ans <- utils::read.csv(temp_output_file,
                           row.names = NULL,
                           stringsAsFactors = FALSE,
                           na.strings = c("NA", "N/A", ""),
                           colClasses = rep("character", 8),
                           nrows = num_pat)
  }

  # delete temporary storage file (if necessary)
  if (is.null(output_file)) {
    file.remove(temp_output_file);
  }

  # return answer
  return(ans)
}
