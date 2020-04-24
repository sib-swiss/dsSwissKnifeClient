test_that("dssCut works", {
  dssSubset('iris2', 'iris', '1==1', datasources = opals["local1"])
  dssCut('Sepal.Length',  df = 'iris2', in.place = TRUE, breaks = 2, labels = c('low', 'high'), datasources = opals["local1"])

  x <- dssShowFactors('iris2', datasources = opals["local1"])
  expect_equal(x$local1$Sepal.Length, c('low', 'high'))
})
