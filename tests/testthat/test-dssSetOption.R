test_that("dssSetOption and dssGetOption work", {
  x <- dssSetOption(list(stringsAsFactors = FALSE), datasources = opals)
  expect_true(x[[1]])
  expect_false(dssGetOption('stringsAsFactors')[[1]][[1]])
  #expect_equal(getOption('digits'),4)
})
