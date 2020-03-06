test_that("dssSetOption works", {
  x <- dssSetOption(list(stringsAsFactors = FALSE), datasources = opals)
  expect_true(x[[1]])
  expect_false(getOption('stringsAsFactors'))
  #expect_equal(getOption('digits'),4)
})
