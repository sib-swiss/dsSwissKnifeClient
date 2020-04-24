test_that("dssShowFactors works", {
  rem <- dssShowFactors('iris', datasources = opals)
  expect_equal(rem$local2$Species, c('setosa', 'versicolor', 'virginica'))
})

