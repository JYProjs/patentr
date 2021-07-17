# input data
y1976w1 <- readr::read_csv("data-raw/y1976w1.csv")

# add to package
usethis::use_data(y1976w1)
