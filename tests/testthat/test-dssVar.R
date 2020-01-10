test_that("dssVar split works", {
  rem <- dssVar('iris$Sepal.Length', type = 'split', datasources = opals)
  expect_equal(rem$local1, var(part_iris_1$Sepal.Length))
  expect_equal(rem$local2, var(part_iris_2$Sepal.Length))
})

test_that("dssCov combined works", {
  rem <- dssVar('iris$Sepal.Length', type = 'combine', datasources = opals)
  expect_equal(rem$global, var(iris$Sepal.Length))
})
