test_that("test TXT conversion", {
  # convert sample file w/ internal function
  tester <- txt_to_df_r(input_file = "TXT-sample-2")
  
  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 2)
  expect_equal(ncol(tester), 9)
  
  # exactly 1 missing value in sample data
  expect_equal(sum(is.na(tester)), 1)
})

test_that("test XML1 conversion", {
  # convert sample file w/ internal function
  tester <- xml1_to_df_r(input_file = "XML1-sample-14")
  
  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 14)
  expect_equal(ncol(tester), 9)
})

test_that("test XML2 conversion", {
  # convert sample file w/ internal function
  tester <- xml2_to_df(input_file = "XML2-sample-10")
  
  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 10)
  expect_equal(ncol(tester), 9)
})
