# user facing function to get patent data from USPTO
#' Get Bulk Patent Data from USPTO
#'
#' Download and convert bulk patent data to tidy format from the
#' USPTO website <https://bulkdata.uspto.gov>. Data can be returned as a data
#' frame or written to a file (see `output_file` parameter). Since USPTO issues
#' patents weekly, at minimum, all patents from a given week must be acquired
#' at once.
#'
#' @param year integer vector containing years from which patents should be
#'   collected
#' @param week integer vector of weeks within the corresponding `year` element
#'   from which patents should be collected
#' @param output_file variable of class `character`; will output to that file
#'   in CSV format
#' @return either `TRUE` (placeholder) or object of class `data.frame` (see
#'   param `output_file` for details)
#' @export
#' @importFrom rlang .data
#' @examples
#' ## NOTE: none of the examples are run due to the download requirement
#' \dontrun{
#' # download patents from the first week of 1976 and get data frame
#' patent_data <- get_bulk_patent_data(year = 1976, week = 1)
#'
#' # download patents from the last 5 weeks of 1980 (and write to a file)
#' get_bulk_patent_data(year = rep(1980, 5), week = 48:52,
#'                      output_file = "patent-data.csv")
#' }
get_bulk_patent_data <- function(year, week, output_file) {
  # valid arguments?
  if (sum(is.na(year)) > 0 | sum(is.na(week)) > 0 |
      sum(is.null(year)) > 0 | sum(is.null(week)) > 0) {
    stop("`year` and `week` parameters cannot have missing values")
  }
  if (length(year) != length(week)) {
    stop(paste("`year` and `week` parameters should have equal length:",
               "year =", year, "& week =", week),
         call. = FALSE)
  }
  if (!(is.numeric(year) & is.numeric(week))) {
    stop(paste("`year` and `week` parameters must be numbers:",
               "class(year) =", class(year), "& class(week) =", class(week)),
         call. = FALSE)
  }
  year <- as.integer(year)
  week <- as.integer(week)
  curr_day <- suppressWarnings(lubridate::today())
  if (sum(year < 1976) > 0) {
    stop(paste("Cannot get patent data prior to 1976; passed year =",
               year),
         call. = FALSE)
  } else if (sum(year > lubridate::year(curr_day)) > 0) {
    stop(paste("Cannot get patent data from the future; passed year =",
               year, "& today =", curr_day))
  }
  if (sum(week <= 0) > 0 | sum(week > 53) > 0) {
    stop(paste("`week` variable must be valid (between 1 and 53, inclusive);",
               "passed week =", week))
  }
  # add checks to see if week is in the future
  # add checks to see if week is valid for the year (e.g. if 53 and year doesn't have start of 53rd week)
  
  # make appropriate data frame
  date_df <- data.frame(Year = year,
                        Week = week)

  # split into 3 data frames based on format (3 formats used by USPTO, as of Feb 2021)
  date_df_txt <- dplyr::filter(date_df, .data$Year <= 2001)
  date_df_xml1<- dplyr::filter(date_df, .data$Year >= 2002 & .data$Year <= 2004)
  date_df_xml2<- dplyr::filter(date_df, .data$Year >= 2005)
  
  # output header
  cat("WKU,Title,App_Date,Issue_Date,Inventor,Assignee,ICL_Class,References,Claims\n",
      file = output_file)

  # get data for all 3 and return TRUE if all worked
  if (nrow(date_df_txt) > 0) {
    convert_txt(date_df_txt, output_file, header = FALSE)
  }
  if (nrow(date_df_xml1) > 0) {
    convert_xml1(date_df_xml1, output_file, header = FALSE)
  }
  if (nrow(date_df_xml2) > 0) {
    convert_xml2(date_df_xml2, output_file, header = FALSE)
  }
  
  # convert WKU to patent numbers (make sure nothing was ruined in the process)
  # only do if non-empty to avoid errors
  
  TRUE
}
