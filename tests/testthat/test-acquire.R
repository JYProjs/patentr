test_that("empty parameters return empty result", {
  # empty parameters return empty output
  # (can't test real parameters b/c internet dependent)
  get_bulk_patent_data(numeric(0), numeric(0), output_file = "test-output.csv")
  tester <- readr::read_csv("test-output.csv")
  file.remove("test-output.csv")

  expect_equal(nrow(tester), 0)
  expect_equal(ncol(tester), 9)
})

test_that("errors are thrown appropriately", {
  # NAs in parameters
  expect_error(get_bulk_patent_data(NA, 5, output_file = "tester.csv"))
  expect_error(get_bulk_patent_data(1980, NA, output_file = "tester.csv"))

  # differing parameter lengths
  expect_error(get_bulk_patent_data(1980, 1:2, output_file = "tester.csv"))
  expect_error(get_bulk_patent_data(1980:1981, 1, output_file = "tester.csv"))

  # non-numeric parameter
  expect_error(get_bulk_patent_data("1976", 1, output_file = "tester.csv"))
  expect_error(get_bulk_patent_data(1976, "1", output_file = "tester.csv"))

  # parameters not within valid range
  expect_error(get_bulk_patent_data(1975, 1, output_file = "tester.csv"))
  expect_error(get_bulk_patent_data(2050, 4, output_file = "tester.csv"))
  expect_error(get_bulk_patent_data(1980, 0, output_file = "tester.csv"))
  expect_error(get_bulk_patent_data(1980, 54, output_file = "tester.csv"))
  
  # make sure intermediate file wasn't created
  expect_false(file.exists("tester.csv"))
})
