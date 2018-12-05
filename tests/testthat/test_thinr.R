context("thinr")

## import data
data("mx_2015")
mx_thin <- thinr(mx_2015)


test_that("thinr works as expected", {
  expect_is(mx_thin, "data.frame")
  expect_lt(nrow(mx_thin), nrow(mx_2015))
})
