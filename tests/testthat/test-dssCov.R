test_that("dssCov split works", {
  rem <- dssCov('iris', type = 'split', datasources = opals)
  expect_equal(rem$server1$vcov, cov(session1$iris[,1:4]))
  expect_equal(rem$server2$vcov, cov(session2$iris[,1:4]))
})

test_that("dssCov combined works", {
  rem <- dssCov('iris', type = 'combine', datasources = opals)
  expect_equal(rem$global$vcov, cov(iris[,1:4]))
})
