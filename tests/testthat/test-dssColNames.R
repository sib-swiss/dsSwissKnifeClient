test_that("dssColNames works", {
  orig <- dssColNames('iris', datasources = opals)
  dssColNames('iris', value =  c('sl', 'sw', 'pl', 'pw', 'sp'), datasources = opals)
  expect_equal(dssColNames('iris', datasources = opals)[[1]], c('sl', 'sw', 'pl', 'pw', 'sp') )
  dssColNames('iris', orig[[1]], datasources = opals)
  expect_equal(dssColNames('iris', datasources = opals)[[2]], orig[[1]])
   })
