test_that("dssAddDaysInterval works", {
  x <- seq.Date(from = as.Date('2010-10-10'), to= as.Date('2020-01-01'), length.out = 40)
  y <- x + 5
  opals$local1$envir$iris_dates <- data.frame(opals$local1$envir$iris, st = as.character(x), en = as.character(y))
  dssAddDaysInterval('iris_interval', 'iris_dates', list(elapsed = list(end_date = 'en', start_date = 'st')), datasources = opals["local1"])
  expect_equal(opals$local1$envir$iris_interval$elapsed, rep(5,40))
})
