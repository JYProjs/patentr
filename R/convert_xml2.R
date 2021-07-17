# convert multiple XML2 files to CSV
# internal function
# date_df: column1 = year; column2 = week
# output_file should be a CSV
# returns TRUE if successful, FALSE otherwise
convert_xml2 <- function(date_df,
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
      cat("DOWNLOADING XML2 FILE ", row_num, "...", sep = "")
      xml2_to_csv(date_df$Year[row_num], date_df$Week[row_num],
                  output_file = output_file)
      cat("DONE\n")
      TRUE
    }
  ) %>%
    unlist
  
  # return TRUE only if all of the downloads + conversions were successful
  ans %>% all
}

# converts single XML2 file to CSV
# always appends b/c output_file contains at least the header row
# year is always a single year (not vector w/ multiple); same for week param
xml2_to_csv <- function(year, week, output_file) {
  # download bulk file from USPTO
  temp_filename <- "temp_ans.xml"
  download_uspto(year = year, week = week, destfile = temp_filename)
  cat("PROCESSING...")
  
  # convert downloaded file
  xml2_to_csv_base(xml2_file = temp_filename,
                   csv_con = output_file)
  
  # delete downloaded file
  file.remove(temp_filename)
}

# actually does work to convert XML1 to CSV
xml2_to_csv_base <- function(xml2_file, csv_con, append = FALSE) {
  if (!append) {
    cat("", file = csv_con, append = FALSE)
  }
  
  # scope out file being converted
  pat_sizes <- get_xml_sizes(xml2_file)
  num_pats <- length(pat_sizes)
  
  # setup progress bar
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = num_pats)
  pb$tick(0)
  
  # setup input
  curr_patrow <- 1
  curr_patxml <- ""
  con <- file(xml2_file, "r")
  while (curr_patrow <= num_pats) {
    # read as much as necessary for current patent
    curr_patxml <- readLines(con, n = pat_sizes[curr_patrow]) %>%
      paste0(collapse = "")
    
    # fix current patent w/ start and end tags
    curr_patxml <- paste0("<start>", curr_patxml, "</start>")
    
    ## process current patent
    curr_xml <- xml2::read_html(curr_patxml)
    WKU <- curr_xml %>%
      xml2::xml_find_first(".//us-patent-grant//publication-reference//document-id//doc-number") %>%
      xml2::xml_text() %>%
      format_field_df()
    title <- curr_xml %>%
      xml2::xml_find_first(".//us-patent-grant//invention-title") %>%
      xml2::xml_text() %>%
      format_field_df() %>%
      remove_csv_issues()
    app_date <- curr_xml %>%
      xml2::xml_find_first(".//us-patent-grant//application-reference//date") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    issue_date <- curr_xml %>%
      xml2::xml_find_first(".//us-patent-grant//publication-reference//date") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    
    icl_class <- curr_xml %>%
      xml2::xml_find_all(".//us-patent-grant//classification-ipc//main-classification") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_ipc) {
               curr_ipc %>%
                 xml2::xml_text()
             }) %>%
      paste0(collapse = ";")
    
    claims <- curr_xml %>%
      xml2::xml_find_all(".//us-patent-grant//claims//claim//claim-text") %>%
      xml2::xml_text() %>%
      gsub(pattern = "\"", replacement = "", fixed = TRUE) %>%
      paste0(collapse = " ") %>%
      remove_csv_issues()
    
    # extract inventor
    inventor <- curr_xml %>%
      xml2::xml_find_all(".//us-patent-grant//applicants//applicant//addressbook") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_inv) {
               curr_first <- curr_inv %>%
                 xml2::xml_find_first(".//first-name") %>%
                 xml2::xml_text()
               
               curr_last <- curr_inv %>%
                 xml2::xml_find_first(".//last-name") %>%
                 xml2::xml_text()
               
               paste(curr_first, curr_last)
             }) %>%
      paste0(collapse = ";") %>%
      remove_csv_issues()
    
    # extract assignee
    assignee <- curr_xml %>%
      xml2::xml_find_all(".//us-patent-grant//assignees//assignee") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_assign) {
               curr_assign %>%
                 xml2::xml_find_first(".//addressbook//orgname") %>%
                 xml2::xml_text()
             }) %>%
      paste0(collapse = ";") %>%
      remove_csv_issues()
    
    # extract references
    references <- curr_xml %>%
      xml2::xml_find_all(".//us-patent-grant//references-cited//citation//patcit") %>%
      vapply(USE.NAMES = FALSE,
             FUN.VALUE = character(1),
             FUN = function(curr_pcit_xml) {
               # if foreign, return blank
               check_foreign <- curr_pcit_xml %>%
                 xml2::xml_find_first(".//country") %>%
                 xml2::xml_text()
               if (check_foreign != "US") return("")
               
               # if not foreign, return XML text
               ans <- curr_pcit_xml %>%
                 xml2::xml_find_first(".//doc-number") %>%
                 xml2::xml_text() %>%
                 strip_nonalphanum()
               
               return(ans)
             }) %>%
      paste0(collapse = ";") %>%
      gsub(pattern = ";;+", replacement = ";")
    
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
