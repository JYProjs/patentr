# convert multiple XML1 files
# internal function
# date_df: column1 = year; column2 = week
# output_file should be a CSV
# returns data frame of patent data of output_file == NULL; TRUE otherwise
# if:
#   - output_file = NULL is used to acquire data and store into `df1`
#   - output_file = <filename> is used to acquire data and then read into `df2` with `read.csv`
# then: all.equal(df1, df2) should return TRUE
# append true b/c potentially adding on to txt
convert_xml1_to_df <- function(date_df, output_file = NULL, append = TRUE) {
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
  if (!is.null(output_file) & !append) {
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
    
    # convert uncompressed file
    curr_df <- xml1_to_df_r(input_file = curr_file,
                            output_file = output_file,
                            append = append)
    
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

# convert XML1 file containing patent data to data frame
xml1_to_df_r <- function(input_file, output_file = NULL, append = FALSE) {
  # convert XML1 to CSV
  ans <- xml1_to_df_base(input_file)
  
  # if necessary, output CSV, otherwise just return
  if (is.null(output_file)) {
    return(ans)
  } else {
    utils::write.csv(x = ans,
                     file = output_file,
                     row.names = FALSE,
                     append = append,
                     col.names = !append)
    
    return(TRUE)
  }
}

# "WKU,Title,App_Date,Issue_Date,Inventor,Assignee,ICL_Class,References\n"
# don't need extra parameters b/c within R
#' @import magrittr
xml1_to_df_base <- function(input_file) {
  # setup data frame
  pat_sizes <- get_xml_sizes(input_file)
  num_pats <- length(pat_sizes)
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
  curr_patrow <- 1
  search_term <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  curr_patxml <- ""
  con <- file(input_file, "r")
  while (curr_patrow <= num_pats) {
    # read as much as necessary for current patent
    curr_patxml <- readLines(con, n = pat_sizes[curr_patrow]) %>%
      paste0(collapse = "")
    
    # fix current patent w/ start and end tags
    curr_patxml <- paste0("<start>", curr_patxml, "</start>")
    
    ## process current patent
    curr_xml <- xml2::read_html(curr_patxml)
    ans$WKU[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b110") %>%
      xml2::xml_text() %>%
      format_field_df()
    ans$Title[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b540") %>%
      xml2::xml_text() %>%
      format_field_df()
    ans$App_Date[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b220") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    ans$Issue_Date[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b140") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    ans$ICL_Class[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b511") %>%
      xml2::xml_text() %>%
      format_field_df()
    
    # get references (after removing foreign ones)
    ans$References[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//pcit") %>%
      vapply(FUN.VALUE = character(1),
             FUN = function(curr_pcit_xml) {
               # if foreign, return NA
               check_foreign <- curr_pcit_xml %>%
                 xml2::xml_find_all(".//ctry")
               if (length(check_foreign) > 0) return("")
               
               # if not foreign, return XML text
               ans <- curr_pcit_xml %>%
                 xml2::xml_find_all(".//dnum") %>%
                 xml2::xml_text() %>%
                 strip_nonalphanum()
               
               return(ans)
             }) %>%
      paste0(collapse = ";") %>%
      gsub(pattern = ";;+", replacement = ";")
    
    #ans$References[curr_patrow] <- curr_xml %>%
    #  xml2::xml_find_all(".//pcit//dnum") %>%
    #  xml2::xml_text() %>%
    #  format_field_df()
      
    # get assignee
    ans$Assignee[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b731//nam") %>%
      vapply(FUN.VALUE = character(1),
             FUN = function(curr_assign) {
               xml2::xml_text(curr_assign)
             }) %>%
      paste0(collapse = ";")
    
    # get inventor
    ans$Inventor[curr_patrow] <- curr_xml %>%
      xml2::xml_find_all(".//b721//nam") %>%
      vapply(FUN.VALUE = character(1),
             FUN = function(curr_inv) {
               curr_first <- curr_inv %>%
                 xml2::xml_find_all(".//fnm") %>%
                 xml2::xml_text()
               
               curr_last <- curr_inv %>%
                 xml2::xml_find_all(".//snm") %>%
                 xml2::xml_text()
               
               paste(curr_first, curr_last)
             }) %>%
      paste0(collapse = ";")
    
    # update necessary variables
    print(paste("FINISHED PATENT", curr_patrow, "OUT OF", num_pats))
    curr_patrow <- curr_patrow + 1
    curr_patxml <- ""
  }
  close(con)
  
  # return data frame
  return(ans)
}
