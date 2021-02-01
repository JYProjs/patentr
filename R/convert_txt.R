# internal function to convert txt bulk data
# input = file to convert (year & week)
# output = data frame
convert_txt_to_df <- function(year, week) {
  # year has to be between 1976 and 2001 (inclusive)
  if (year < 1976 | year > 2001) {
    stop(paste("Patents from the year", year,
               "are not published in TXT format."),
         call. = FALSE)
  }

  # download data - NEED TO DO

  # extract/uncompress downloaded data - NEED TO DO
  data_file <- ""

  # convert TXT file
  ans <- convert_txt_to_df(data_file)

  # return data frame
  return(ans)
}

# convert TXT file containing patent data to data frame
# NEED TO REMOVE SECOND PARAMETER DEFAULT VALUE - maybe preserve?
#' @export
convert_txt_to_df <- function(input_file, temp_output_file = "temp-output.csv") {
  # convert TXT to CSV
  num_pat <- txt_to_df_cpp(input_file, temp_output_file)

  # read CSV as data frame and return
  ans <- utils::read.csv(temp_output_file,
                         row.names = NULL,
                         stringsAsFactors = FALSE,
                         na.strings = c("NA", "N/A", ""),
                         colClasses = rep("character", 8),
                         nrows = num_pat)

  # delete temporary storage file
  file.remove(temp_output_file);

  # return answer
  return(ans)
}
