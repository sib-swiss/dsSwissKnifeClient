test_that("dssMean works", {
  rem <- dssMean('iris$Sepal.Length', datasources = opals)
  expect_equal(rem$server1, mean(session1$iris$Sepal.Length))
  expect_equal(rem$server2, mean(session2$iris$Sepal.Length))
  expect_equal(rem$global, mean(iris$Sepal.Length))
})
