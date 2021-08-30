test_that("dssSetOption and dssGetOption work", {
  x <- dssSetOption(list(stringsAsFactors = FALSE))
  expect_true(x[[1]])
  expect_false(dssGetOption('stringsAsFactors', datasources = opals)[[1]][[1]])

})

test_that("dssSetOption and dssGetOption work", {
  x <- dssSetOption(list(allowed.functions = TRUE))
  expect_true(x[[1]])
  expect_null(dssGetOption('allowed.functions', datasources = opals)[[1]][[1]])

})
