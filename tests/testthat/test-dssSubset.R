test_that("dssSubset works", {
  dssSubset('iris_filtered', 'iris', row.filter = 'Sepal.Length < 6 & Species == "setosa"', col.filter = '!(colnames(iris) == "Petal.Width")', datasources = opals[2])
  iris_filtered2 <- part_iris_2[part_iris_2$Sepal.Length < 6 & part_iris_2$Species == "setosa", !(colnames(part_iris_2) == "Petal.Width")]
  iris_filtered2$Species <- droplevels(iris_filtered2$Species)
  row.names(iris_filtered2) <- 1:10
  expect_equal(test$locals$local2$envir$iris_filtered, iris_filtered2)

})
