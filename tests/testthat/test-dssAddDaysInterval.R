test_that("dssAddDaysInterval works", {
  x <- seq.Date(from = as.Date('2010-10-10'), to= as.Date('2020-01-01'), length.out = 75)
  y <- x + 5
  session1$iris_dates <- data.frame(session1$iris, st = as.character(x), en = as.character(y))
  dssAddDaysInterval('iris_interval', 'iris_dates', list(elapsed = list(end_date = 'en', start_date = 'st')), datasources = opals["server1"])
  expect_equal(session1$iris_interval$elapsed, rep(5,75))
})
