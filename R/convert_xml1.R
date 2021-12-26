# convert multiple XML1 files to CSV
# internal function
# date_df: column1 = year; column2 = week
# output_file should be a CSV
# returns TRUE if successful, FALSE otherwise
convert_xml1 <- function(date_df,
                         output_file, # output_file needs to be a connection to simplify things
                         header = TRUE) {
  ## TO DO: confirm valid parameters
  
  # create header for output file (if necessary)
  if (header) {
    cat("WKU,Title,App_Date,Issue_Date,Inventor,Assignee,ICL_Class,References,Claims\n",
        file = output_file)
  }
  
  # loop through date_df
  ans <- vapply(
    X = seq_len(nrow(date_df)),
    USE.NAMES = FALSE,
    FUN.VALUE = logical(1),
    FUN = function(row_num) {
      cat("DOWNLOADING XML1 FILE ", row_num, "...", sep = "")
      xml1_to_csv(date_df$Year[row_num], date_df$Week[row_num],
                  output_file = output_file)
      cat("DONE\n")
      TRUE
    }
  ) %>%
    unlist()
  
  # return TRUE only if all of the downloads + conversions were successful
  ans %>% all
}

# converts single XML1 file to CSV
# always appends b/c output_file contains at least the header row
# year is always a single year (not vector w/ multiple); same for week param
xml1_to_csv <- function(year, week, output_file) {
  ## download bulk file from USPTO
  temp_filename <- "temp_ans.xml"
  download_uspto(year = year, week = week, destfile = temp_filename)
  cat("PROCESSING...")
  
  ## convert downloaded file
  xml1_to_csv_base(xml1_file = temp_filename,
                   csv_con = output_file)
  
  ## delete downloaded file
  file.remove(temp_filename)
}

# actually does work to convert XML1 to CSV
xml1_to_csv_base <- function(xml1_file, csv_con, append = FALSE) {
  # create new file if not appending
  if (!append) {
    cat("", file = csv_con, append = FALSE)
  }
  
  # scope out file being converted
  pat_sizes <- get_xml_sizes(xml1_file)
  num_pats <- length(pat_sizes)
  
  # setup progress bar
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = num_pats)
  pb$tick(0)
  
  # setup input
  curr_patrow <- 1
  curr_patxml <- ""
  con <- file(xml1_file, "r")
  while (curr_patrow <= num_pats) {
    # read as much as necessary for current patent
    curr_patxml <- readLines(con, n = pat_sizes[curr_patrow]) %>%
      paste0(collapse = "")
    
    # fix current patent w/ start and end tags
    curr_patxml <- paste0("<start>", curr_patxml, "</start>")
    
    ## process current patent
    skipped <- FALSE
    curr_xml <- tryCatch(
      { xml2::read_html(curr_patxml) %>% suppressWarnings() },
      error = function(e) {
        skipped <<- TRUE
        print(paste("SKIPPED A PATENT:", e))
      }
    )
    
    # skip patent due to (most likely) incorrect XML formatting
    if (skipped) {
      pb$tick()
      curr_patrow <- curr_patrow + 1
      curr_patxml <- ""
      next
    }
    
    WKU <- curr_xml %>%
      xml2::xml_find_first(".//b110") %>%
      xml2::xml_text() %>%
      format_field_df()
    title <- curr_xml %>%
      xml2::xml_find_first(".//b540") %>%
      xml2::xml_text() %>%
      format_field_df() %>%
      remove_csv_issues()
    app_date <- curr_xml %>%
      xml2::xml_find_first(".//b220") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    issue_date <- curr_xml %>%
      xml2::xml_find_first(".//b140") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    icl_class <- curr_xml %>%
      xml2::xml_find_all(".//b511") %>%
      xml2::xml_text() %>%
      format_field_df()
    claims <- curr_xml %>%
      xml2::xml_find_all(".//cl//clm//ptext") %>%
      xml2::xml_text() %>%
      gsub(pattern = "\"", replacement = "", fixed = TRUE) %>%
      paste0(collapse = " ") %>%
      remove_csv_issues()
    
    # get references (after removing foreign ones)
    references <- curr_xml %>%
      xml2::xml_find_all(".//pcit") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_pcit_xml) {
               # if foreign, return NA
               check_foreign <- curr_pcit_xml %>%
                 xml2::xml_find_first(".//ctry")
               if (length(check_foreign) > 0) return("")
               
               # if not foreign, return XML text
               ans <- curr_pcit_xml %>%
                 xml2::xml_find_first(".//dnum") %>%
                 xml2::xml_text() %>%
                 strip_nonalphanum()
               
               return(ans)
             }) %>%
      paste0(collapse = ";") %>%
      gsub(pattern = ";;+", replacement = ";")
    
    # get assignee
    assignee <- curr_xml %>%
      xml2::xml_find_all(".//b731//nam") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_assign) {
               xml2::xml_text(curr_assign)
             }) %>%
      paste0(collapse = ";") %>%
      remove_csv_issues()
    
    # get inventor
    inventor <- curr_xml %>%
      xml2::xml_find_all(".//b721//nam") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_inv) {
               curr_first <- curr_inv %>%
                 xml2::xml_find_first(".//fnm") %>%
                 xml2::xml_text()
               
               curr_last <- curr_inv %>%
                 xml2::xml_find_first(".//snm") %>%
                 xml2::xml_text()
               
               paste(curr_first, curr_last)
             }) %>%
      paste0(collapse = ";") %>%
      remove_csv_issues()
    
    # output to file in CSV format
    cat(paste0("\"",WKU,"\",",
               "\"",title,"\",",
               app_date,",",
               issue_date,",",
               "\"",inventor,"\",",
               "\"",assignee,"\",",
               "\"",icl_class,"\",",
               "\"",references,"\",",
               "\"",claims,"\"\n"),
        file = csv_con,
        append = TRUE)
    
    # update loop vars
    pb$tick()
    curr_patrow <- curr_patrow + 1
    curr_patxml <- ""
  }
  close(con)
}
