test_that("empty parameters return empty result", {
  # empty parameters return empty output
  # (can't test real parameters b/c internet dependent)
  a <- get_bulk_patent_data(numeric(0), numeric(0))

  expect_equal(nrow(a), 0)
  expect_equal(ncol(a), 0)
})

test_that("errors are thrown appropriately", {
  # NAs in parameters
  expect_error(get_bulk_patent_data(NA, 5))
  expect_error(get_bulk_patent_data(1980, NA))

  # differing parameter lengths
  expect_error(get_bulk_patent_data(1980, 1:2))
  expect_error(get_bulk_patent_data(1980:1981, 1))

  # non-numeric parameter
  expect_error(get_bulk_patent_data("1976", 1))
  expect_error(get_bulk_patent_data(1976, "1"))

  # parameters not within valid range
  expect_error(get_bulk_patent_data(1975, 1))
  expect_error(get_bulk_patent_data(2050, 4))
  expect_error(get_bulk_patent_data(1980, 0))
  expect_error(get_bulk_patent_data(1980, 54))
})
