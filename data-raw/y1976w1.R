# input data
y1976w1 <- read.csv("data-raw/y1976w1.csv")

# add to package
usethis::use_data(y1976w1, overwrite = TRUE)
