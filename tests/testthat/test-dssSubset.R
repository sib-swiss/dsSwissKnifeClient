test_that("dssSubset works", {
  dssSubset('iris_filtered', 'iris', row.filter = 'Sepal.Length < 6 & Species == "setosa"', col.filter = '!(colnames(iris) == "Petal.Width")', datasources = opals["server2"])
  iris_filtered2 <- session2$iris[session2$iris$Sepal.Length < 6 & session2$iris$Species == "setosa", !(colnames(session2$iris) == "Petal.Width")]
  iris_filtered2$Species <- droplevels(iris_filtered2$Species)
  row.names(iris_filtered2) <- as.numeric(rownames(iris_filtered2))
  expect_equal(session2$iris_filtered, iris_filtered2)

})

