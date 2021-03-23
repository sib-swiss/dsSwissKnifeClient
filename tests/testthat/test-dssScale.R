test_that("dssScale_TRUE_TRUE works", {
  dssScale('iris_scaled', 'iris', center = TRUE, scale = TRUE, datasources = opals)
  x <- as.data.frame(scale(iris[,1:4],center = TRUE, scale=TRUE))
  expect_equal(round(rbind(session1$iris_scaled[,2:5],session2$iris_scaled[,2:5] ),2), round(x,2))
  xdef <- as.data.frame(scale(iris[,1:4]))
  dssScale('iris_scaled_def', 'iris',  datasources = opals)
  expect_equal(round(rbind(session1$iris_scaled_def[,2:5],session2$iris_scaled_def[,2:5] ),2), round(xdef,2))
})
test_that("dssScale_FALSE_FALSE works", {
  dssScale('iris_scaled', 'iris', center = FALSE, scale = FALSE, datasources = opals)
  x <- as.data.frame(scale(iris[,1:4],center = FALSE, scale=FALSE))
  expect_equal(round(rbind(session1$iris_scaled[,2:5],session2$iris_scaled[,2:5] ),2), round(x,2))
})

test_that("dssScale_TRUE_FALSE works", {
  dssScale('iris_scaled', 'iris', center = TRUE, scale = FALSE, datasources = opals)
  x <- as.data.frame(scale(iris[,1:4],center = TRUE, scale=FALSE))
  expect_equal(round(rbind(session1$iris_scaled[,2:5],session2$iris_scaled[,2:5] ),2), round(x,2))
})

test_that("dssScale_FALSE_TRUE works", {
  dssScale('iris_scaled', 'iris', center = FALSE, scale = TRUE, datasources = opals)
  x <- as.data.frame(scale.default(iris[,1:4],center = FALSE, scale=TRUE))
  expect_equal(round(rbind(session1$iris_scaled[,2:5],session2$iris_scaled[,2:5] ),2), round(x,2))
})

test_that("dssScale_SOMETHING_SOMETHING works", {
  dssScale('iris_scaled', 'iris', center = 1:4, scale = 1:4, datasources = opals)
  x <- as.data.frame(scale.default(iris[,1:4],center = 1:4, scale=1:4))
  expect_equal(round(rbind(session1$iris_scaled[,2:5],session2$iris_scaled[,2:5] ),2), round(x,2))
})

