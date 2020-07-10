test_that("dssShowFactors works", {
  rem <- dssShowFactors('iris')
  expect_equal(rem$server2$Species, c('setosa', 'versicolor', 'virginica'))
})

