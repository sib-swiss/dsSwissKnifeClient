test_that("dssDeriveColumn works", {
  dssSubset('iris_der', 'iris', row.filter = '1==1')
   dssDeriveColumn('iris_der', 'new_col', 'Sepal.Length/round(2.2)')
  data('iris', envir = environment())
  iris$new_col <- iris$Sepal.Length/round(2.2)
  x <- dssMean('iris_der$new_col', datasources = opals)
  y <- mean(iris$new_col)
  expect_equal(x$global, y)
  expect_match({
        try(dssDeriveColumn('iris_der', 'new_col', 'Sepal.Length[1]'), silent = TRUE)
        datashield.errors()[[1]]
        }, regexp = 'individual elements')

  expect_match({
    try(dssDeriveColumn('iris_der', 'new_col', 'rnorm(length(iris$Sepal.Length), mean = mean(iris$Sepal.Length),1)', datasources = opals), silent = TRUE)
     datashield.errors()[[1]]
     }, regexp = 'rnorm, mean not allowed here')

})
