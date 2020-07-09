test_that("dssScale works", {
  dssScale('iris_scaled', 'iris', datasources = opals["server1"])
  x <- as.data.frame(scale(session1$iris[,1:4]))
  expect_equal(summary(session1$iris_scaled[,2:5]), summary(x))
})
