test_that("dssVar split works", {
  rem <- dssVar('iris$Sepal.Length', type = 'split', datasources = opals)
  expect_equal(rem$server1$var, var(session1$iris$Sepal.Length))
  expect_equal(rem$server2$var, var(session2$iris$Sepal.Length))
})



test_that("dssCov combined works", {
  rem <- dssVar('iris$Sepal.Length', type = 'combine', datasources = opals)
  expect_equal(rem$global$var, var(iris$Sepal.Length))
})


