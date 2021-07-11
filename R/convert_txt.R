# convert multiple TXT files to CSV
# internal function
# date_df: column1 = year; column2 = week
# output file should be a CSV
# returns TRUE if successful, FALSE otherwise
convert_txt <- function(date_df,
                        output_file, # output_file needs to be a connection?
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
      cat("DOWNLOADING TXT FILE ", row_num, "...", sep = "")
      txt_to_csv(date_df$Year[row_num], date_df$Week[row_num],
                 output_file = output_file)
      cat("DONE\n")
      TRUE
    }
  )
  
  # return TRUE only if all of the downloads + conversions were successful
  ans %>% all
}

# converts single TXT file to CSV
# always appends b/c output_file contains at least the header row
# year is always a single year (not vector w/ multiple); same for week param
txt_to_csv <- function(year, week, output_file) {
  ## download bulk file from USPTO
  temp_filename <- "temp_ans.txt"
  download_uspto(year = year, week = week, destfile = temp_filename)
  cat("PROCESSING...")
  
  ## convert downloaded file
  txt_to_df_cpp(temp_filename, output_file, TRUE, FALSE)
  
  ## delete downloaded file
  file.remove(temp_filename)
}
