test_that("dssPrincomp split works", {
  rem <- dssPrincomp('iris', type = 'split', datasources = opals)
  pr1 <- princomp(part_iris_1[,1:4])
  pr2 <- princomp(part_iris_2[,1:4])
  expect_true(any(abs(rem$local1$loadings) - abs(pr1$loadings) < 1e-5))
  expect_true(any(abs(rem$local2$loadings) - abs(pr2$loadings) < 1e-5))
})

test_that("dssPrinComp combined works", {
  rem <- dssPrincomp('iris', type = 'combine', datasources = opals)
  pr <- princomp(iris[,1:4])
  expect_true(any(abs(rem$global$loadings) - abs(pr$loadings) < 1e-5))
})

test_that("biplot combined works", {
  rem <- dssPrincomp('iris', type = 'combine', datasources = opals)
  pr <- princomp(iris[,1:4])
  expect_null(biplot(rem$global, levels = 'iris_scores$Species', datasources = opals))
})
