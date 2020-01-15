test_that("dssScale works", {
  dssScale('iris_scaled', 'iris', datasources = opals[1])
  x <- as.data.frame(scale(test$locals$local1$envir$iris[,1:4]))
  expect_equal(summary(test$locals$local1$envir$iris_scaled[,2:5]), summary(x))
})
