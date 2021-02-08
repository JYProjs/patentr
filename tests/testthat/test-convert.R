test_that("test TXT conversion", {
  # convert sample file w/ internal function
  tester <- txt_to_df_r(input_file = "TXT-sample-2")

  ## confirm output accuracy
  # basic dimensions
  expect_equal(nrow(tester), 2)
  expect_equal(ncol(tester), 8)

  # exactly 1 missing value in sample data
  expect_equal(sum(is.na(tester)), 1)
})
