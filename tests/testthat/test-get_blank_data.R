context("test-get_blank_data")


test_that("getBlankData returns a blanks datatable", {
  library(lubridate)
  from = today() - weeks(1)
  blanks <- getBlankData(from)
  expect_that(blanks, is_a('data.frame'))
})
