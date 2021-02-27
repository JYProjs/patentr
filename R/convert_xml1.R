# convert multiple XML1 files
# internal function
# date_df: column1 = year; column2 = week
# output_file should be a CSV
# returns data frame of patent data of output_file == NULL; TRUE otherwise
# if:
#   - output_file = NULL is used to acquire data and store into `df1`
#   - output_file = <filename> is used to acquire data and then read into `df2` with `read.csv`
# then: all.equal(df1, df2) should return TRUE
convert_xml1_to_df <- function(date_df, output_file = NULL) {
  # NEED TO COMPLETE: confirm dates and data frame format are valid
  # internal function so should never hit this issue
  if (!("Year" %in% colnames(date_df) & "Week" %in% colnames(date_df))) {
    stop(paste("date_df parameter must have `Year` and `Week` columns;",
               "available cols are:", colnames(date_df)),
         call. = FALSE)
  }
  
  # base vars
  filename_uspto <- "pgYYMMDD"
  xml1_uspto_url <- "https://bulkdata.uspto.gov/data/patent/grant/redbook/fulltext/"
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
      gsub(pattern = "YY", replacement = substr(curr_year, 3, 4), fixed = TRUE) %>%
      gsub(pattern = "MM", replacement = int_with_len(curr_month, 2), fixed = TRUE) %>%
      gsub(pattern = "DD", replacement = int_with_len(curr_day, 2), fixed = TRUE)
    curr_url <- xml1_uspto_url %>%
      paste0(curr_year, "/", curr_file, ".zip")
    curr_file <- paste0(curr_file, ".xml")
    
    # download appropriate zip from USPTO bulk website
    utils::download.file(url = curr_url,
                         destfile = dest_file)
    
    # uncompress
    utils::unzip(zipfile = dest_file)
    
    # delete zip
    file.remove(dest_file)
    
    # convert uncompressed file
    curr_df <- xml1_to_df_r(input_file = curr_file, output_file = output_file)
    
    # delete uncompressed file
    file.remove(curr_file)
    
    # store data frame into list or output based on user preference
    if (is.null(output_file)) {
      df_store[[curr_row]] <- curr_df
    }
  }
  
  # combine all data frames in list
  
  # return
  return(ans)
}

# convert XML1 file containing patent data to data frame
xml1_to_df_r <- function(input_file, output_file = NULL) {
  # placeholder
  temp_output_file <- ifelse(is.null(output_file),
                             "temp-patent-package-output.csv",
                             output_file)
  append_bool <- ifelse(is.null(output_file), FALSE, TRUE)
  header_bool <- ifelse(is.null(output_file), TRUE, ifelse(file.exists(output_file), FALSE, TRUE))
  
  # convert XML1 to CSV
  df_pat <- xml1_to_df_base(input_file)
  
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
    file.remove(temp_output_file)
  }
  
  # return answer
  return(ans)
}
# "WKU,Title,App_Date,Issue_Date,Inventor,Assignee,ICL_Class,References\n"
# don't need extra parameters b/c within R
xml_to_df_base <- function(input_file) {
  # setup data frame
  num_pats <- count_xml1(input_file)
  ans <- data.frame(WKU = character(num_pats),
                    Title = character(num_pats),
                    App_Date = character(num_pats),
                    Issue_Date = character(num_pats),
                    Inventor = character(num_pats),
                    Assignee = character(num_pats),
                    ICL_Class = character(num_pats),
                    References = character(num_pats),
                    stringsAsFactors = FALSE)
  
  # setup vars
  curr_patrow <- 0
  search_term <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  curr_patxml <- ""
  con <- file(input_file, "r")
  while (curr_patrow <= num_pats) {
    # read as much as necessary for current patent
    found_end <- FALSE
    while (!found_end) {
      temp_read <- readLines(con, n = 1)
      
      if (length(temp_read) == 0) {
        found_end <- TRUE
      } else if (grepl(x = temp_read, pattern = search_term, fixed = TRUE)) {
        found_end <- TRUE
      } else {
        curr_patxml <- paste0(curr_patxml, temp_read)
      }
    }
    
    # fix current patent w/ start and end tags
    curr_patxml <- paste0("<start>", curr_patxml, "</start>")
    
    ## process current patent
    curr_xml <- xml2::read_html(curr_patxml)
    ans$WKU[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b110") %>%
      xml_text() %>%
      format_field_df()
    ans$Title[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b540") %>%
      xml_text() %>%
      format_field_df()
    ans$App_Date[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b220") %>%
      xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    ans$Issue_Date[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b140") %>%
      xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    ans$ICL_Class[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b511") %>%
      xml_text() %>%
      format_field_df()
    ans$References[curr_patrow] <- curr_xml %>%
      xml_find_all(".//pcit//dnum") %>%
      xml_text() %>%
      format_field_df()
      
    # get assignee
    ans$Assignee[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b731//nam") %>%
      vapply(FUN.VALUE = character(1),
             FUN = function(curr_assign) {
               xml_text(curr_assign)
             }) %>%
      paste0(collapse = ";")
    
    # get inventor
    ans$Inventor[curr_patrow] <- curr_xml %>%
      xml_find_all(".//b721//nam") %>%
      vapply(FUN.VALUE = character(1),
             FUN = function(curr_inv) {
               curr_first <- curr_inv %>%
                 xml_find_all(".//fnm") %>%
                 xml_text()
               
               curr_last <- curr_inv %>%
                 xml_find_all(".//snm") %>%
                 xml_text()
               
               paste(curr_first, curr_last)
             }) %>%
      paste0(collapse = ";")
    
    # update necessary variables
    curr_patrow <- curr_patrow + 1
    curr_patxml <- ""
  }
  close(con)
  
  # return data frame
  return(ans)
}
