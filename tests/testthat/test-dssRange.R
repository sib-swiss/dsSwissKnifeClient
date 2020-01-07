test_that("dssRange split works", {
  rem <- dssRange('iris', type = 'split', datasources = opals)
  r1 <- sapply(part_iris_1[,1:4], range, simplify = FALSE, USE.NAMES = TRUE)
  pr2 <- princomp(part_iris_2[,1:4])
  expect_true(any(abs(rem$local1$loadings) - abs(pr1$loadings) < 1e-5))
  expect_true(any(abs(rem$local2$loadings) - abs(pr2$loadings) < 1e-5))
})

test_that("dssRange combined works", {
  rem <- dssRange('iris', type = 'combine', datasources = opals)
  pr <- princomp(iris[,1:4])
  expect_true(any(abs(rem$global$loadings) - abs(pr$loadings) < 1e-5))
})
