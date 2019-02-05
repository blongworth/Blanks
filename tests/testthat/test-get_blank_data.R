context("test-get_blank_data")

library(lubridate)

from = today() - weeks(1)
test_that("getBlankData returns a blanks datatable", {
  blanks <- getBlankData(from)
  expect_that(blanks, is_a('data.frame'))
})
