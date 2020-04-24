test_that("dssMean works", {
  rem <- dssMean('iris$Sepal.Length', datasources = opals)
  expect_equal(rem$local1, mean(part_iris_1$Sepal.Length))
  expect_equal(rem$local2, mean(part_iris_2$Sepal.Length))
  expect_equal(rem$global, mean(iris$Sepal.Length))
})
