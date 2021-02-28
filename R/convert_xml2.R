convert_xml2_to_df <- function() {
  
}

xml2_to_df <- function(input_file, output_file = NULL, append = FALSE) {
  # convert XML2 to CSV
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
      xml2::xml_find_first(".//us-patent-grant//publication-reference//document-id//doc-number") %>%
      xml2::xml_text() %>%
      format_field_df()
    ans$Title[curr_patrow] <- curr_xml %>%
      xml2::xml_find_first(".//us-patent-grant//invention-title") %>%
      xml2::xml_text() %>%
      format_field_df()
    ans$App_Date[curr_patrow] <- curr_xml %>%
      xml2::xml_find_first(".//us-patent-grant//application-reference//date") %>%
      xml2::xml_text() %>%
      lubridate::as_date() %>%
      as.character() %>%
      format_field_df()
    
    # update necessary vars
    print(paste("FINISHED PATENT", curr_patrow, "OUT OF", num_pats))
    curr_patrow <- curr_patrow + 1
    curr_patxml <- ""
  }
  close(con)
  # at this point, `ans` contains all the converted data-----
  
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
