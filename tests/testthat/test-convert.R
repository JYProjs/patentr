test_that("test TXT conversion", {
  # convert sample file w/ internal function
  txt_to_df_cpp("TXT-sample-2", "test-output.csv", FALSE, TRUE)
  tester <- readr::read_csv("test-output.csv")
  file.remove("test-output.csv")
  
  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 2)
  expect_equal(ncol(tester), 9)
  
  # exactly 1 missing value in sample data
  expect_equal(sum(is.na(tester)), 1)
})

test_that("test XML1 conversion", {
  # convert sample file w/ internal function
  xml1_to_csv_base("XML1-sample-14", "test-output.csv")
  tester <- readr::read_csv("test-output.csv", col_names = FALSE)
  file.remove("test-output.csv")
  
  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 14)
  expect_equal(ncol(tester), 9)
})

test_that("test XML2 conversion", {
  # convert sample file w/ internal function
  xml2_to_csv_base("XML2-sample-10", "test-output.csv")
  tester <- readr::read_csv("test-output.csv", col_names = FALSE)
  file.remove("test-output.csv")
  
  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 10)
  expect_equal(ncol(tester), 9)
})
