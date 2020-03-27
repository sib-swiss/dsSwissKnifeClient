test_that("dssScale works", {
  dssScale('iris_scaled', 'iris', datasources = opals["local1"])
  x <- as.data.frame(scale(opals$local1$envir$iris[,1:4]))
  expect_equal(summary(opals$local1$envir$iris_scaled[,2:5]), summary(x))
})
