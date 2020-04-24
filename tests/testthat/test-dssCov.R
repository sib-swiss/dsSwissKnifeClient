test_that("dssCov split works", {
  rem <- dssCov('iris', type = 'split', datasources = opals)
  expect_equal(rem$local1$vcov, cov(part_iris_1[,1:4]))
  expect_equal(rem$local2$vcov, cov(part_iris_2[,1:4]))
})

test_that("dssCov combined works", {
  rem <- dssCov('iris', type = 'combine', datasources = opals)
  expect_equal(rem$global$vcov, cov(iris[,1:4]))
})
