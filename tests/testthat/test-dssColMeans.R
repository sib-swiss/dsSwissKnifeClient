test_that("dssColMeans split works", {
  rem <- dssColMeans('iris', type = 'split', datasources = opals)
  expect_equal(rem$local1$means, colMeans(part_iris_1[,1:4]))
  expect_equal(rem$local2$means, colMeans(part_iris_2[,1:4]))
})

test_that("dssColMeans combined works", {
  rem <- dssColMeans('iris', type = 'combine', datasources = opals)
  expect_equal(rem$global$means, colMeans(iris[,1:4]))
})
