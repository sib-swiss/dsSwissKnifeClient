test_that("dssRanger ranger works with formula", {
  ranger_model <- dssRanger('ranger', newobj = 'remote_ranger', async = TRUE, datasources = opals, formula = 'Sepal.Length ~ .' ,data ='iris', importance = 'permutation')
  expect_equal(ranger_model$server1$importance.mode, 'permutation')
})


test_that("dssRanger ranger works with x and y", {
  ranger_model <- dssRanger('ranger', newobj = 'remote_ranger', async = TRUE, datasources = opals, x = c('iris$Sepal.Width', 'iris$Sepal.Length'), y = 'iris$Petal.Length', importance = 'permutation')
  expect_equal(ranger_model$server1$importance.mode, 'permutation')
})


test_that("dssRanger ranger works with x as a separate dataframe", {
  dssSubset('ranger_x', 'iris', col.filter = "c('Sepal.Length', 'Sepal.Width')")
  ranger_model <- dssRanger('ranger', newobj = 'remote_ranger', async = TRUE, datasources = opals, x = 'ranger_x', y = 'iris$Petal.Length', importance = 'permutation')
  expect_equal(ranger_model$server1$importance.mode, 'permutation')
})
