test_that("dssColMeans split works", {
  rem <- dssColMeans('iris', type = 'split', datasources = opals)
  expect_equal(rem$server1$means, colMeans(session1$iris[,1:4]))
  expect_equal(rem$server2$means, colMeans(session2$iris[,1:4]))
})

test_that("dssColMeans combined works", {
  rem <- dssColMeans('iris', type = 'combine', datasources = opals)
  expect_equal(rem$global$means, colMeans(iris[,1:4]))
})
