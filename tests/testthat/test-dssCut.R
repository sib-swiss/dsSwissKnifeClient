test_that("dssCut works", {
  dssSubset('iris2', 'iris', '1==1', datasources = opals["server1"], async = FALSE)
  dssCut('Sepal.Length',  df = 'iris2', in.place = TRUE, breaks = 2, labels = c('low', 'high'), datasources = opals["server1"])

  x <- dssShowFactors('iris2', datasources = opals["server1"])
  expect_equal(x$server1$Sepal.Length, c('low', 'high'))
})
