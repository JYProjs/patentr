test_that("utility function: int_with_len", {
  # test single digits
  ans <- paste0("0", 1:9)
  func_out <- vapply(X = 1:9,
                     FUN.VALUE = character(1),
                     FUN = function(i) int_with_len(i, 2))
  expect_equal(ans, func_out)

  # test double digits
  ans <- as.character(10:52)
  func_out <- vapply(X = 10:52,
                     FUN.VALUE = character(1),
                     FUN = function(i) int_with_len(i, 2))
  expect_equal(ans, func_out)

  # should throw error
  expect_error(int_with_len(-1, 2))  # doesn't work for negative numbers
  expect_error(int_with_len(15, 1))  # 15 cannot be represented by a single digit in base 10
})

test_that("utility function: get_date_tues", {
  # test a couple of known dates
  test_dates <- data.frame(Year = 1976:1980,
                           Week = seq(from = 1, to = 21, by = 5),
                           Ans = c("1976-01-06",
                                   "1977-02-08",
                                   "1978-03-14",
                                   "1979-04-17",
                                   "1980-05-20"))
  for (curr_row in seq_len(nrow(test_dates))) {
    func_out_date <- get_date_tues(year = test_dates$Year[curr_row],
                                   week = test_dates$Week[curr_row])

    expect_equal(as.character(func_out_date), test_dates$Ans[curr_row])
  }

  # should throw error
  expect_error(get_date_tues(year = runif(n = 1, min = 1976, max = 2020),
                             week = 0)) # week cannot be less than 1
  expect_error(get_date_tues(year = runif(n = 1, min = 1976, max = 2020),
                             week = 54)) # week cannot be greater than 53
})

test_that("utility function: wku_to_pno", {
  # test w/ example WKUs from 1976
  before <- c("RE028671", "03930271")
  correct<- c("RE28671", "3930271")
  after <- wku_to_pno(before)
  expect_equal(correct, after)
})

test_that("utility function: remove_txt_checksum", {
  # test w/ example numbers from 1976
  before <- c("RE286710", "RE286725", "RE286738",
              "39302710", "39302724", "39302737")
  correct<- c("RE28671", "RE28672", "RE28673",
              "3930271", "3930272", "3930273")
  after <- remove_txt_checksum(before)
  expect_equal(correct, after)
})
