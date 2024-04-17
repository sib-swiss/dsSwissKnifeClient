test_that("dssDeriveColumn works", {
  dssSubset('iris_der', 'iris', row.filter = '1==1')
   dssDeriveColumn('iris_der', 'new_col', 'Sepal.Length/round(2.2)')
   dssDeriveColumn('iris_der', 'rnorm', 'rnorm.0.1()')
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
  expect_lt(abs(mean(session1$iris_der$rnorm)),1 )

})

test_that("dssDeriveColumn one.versus.others work", {
  dssSubset('iris_der', 'iris', row.filter = '1==1')
  dssDeriveColumn('iris_der', 'new_col', 'one.versus.others(Species, "setosa")')
  expect_equal(levels(session1$iris_der$new_col), c('setosa', 'no_setosa'))

})


test_that("dssDeriveColumn merge columns where destination exists work", {
  dssSubset('iris_der', 'iris', row.filter = '1==1')
  session1$iris_der[c(3,5,8,9), 'Sepal.Length'] <- NA
  session1$iris_der[c(3,6,7,9), 'Sepal.Width'] <- NA
  assign('my_dict', list('Sepal.Length' = c('Sepal.Width', 'Petal.Length')), envir = .GlobalEnv)
  dssDeriveColumn('iris_der', 'Sepal.Length', 'mergeConceptIds("Sepal.Length", "my_dict")', datasources = opals[1])
  expect_equal(session1$iris_der[3, 'Sepal.Length'], 1.3)

})

test_that("dssDeriveColumn merge columns where destination doesn't exist work", {
  dssSubset('iris_der', 'iris', row.filter = '1==1')
  session1$iris_der[c(3,5,8,9), 'Sepal.Length'] <- NA
  session1$iris_der[c(3,6,7,9), 'Sepal.Width'] <- NA
  assign('my_dict', list('Sepal.Length1' = c('Sepal.Width', 'Petal.Length')), envir = .GlobalEnv)
  dssDeriveColumn('iris_der', 'Sepal.Length1', 'mergeConceptIds("Sepal.Length1", "my_dict")', datasources = opals[1])
  expect_equal(session1$iris_der[3, 'Sepal.Length1'], 1.3)

})


test_that("dssDeriveColumn merge columns where nothing exists", {
  dssSubset('iris_der', 'iris', row.filter = '1==1')
  session1$iris_der[c(3,5,8,9), 'Sepal.Length'] <- NA
  session1$iris_der[c(3,6,7,9), 'Sepal.Width'] <- NA
  assign('my_dict', list('Sepal.Length1' = c('Sepal.Width1', 'Petal.Length1')), envir = .GlobalEnv)
  dssDeriveColumn('iris_der', 'Sepal.Length1', 'mergeConceptIds("Sepal.Length1", "my_dict")', datasources = opals[1])
  expect_true(is.null(session1$iris_der[3, 'Sepal.Length1']))

})

test_that("dssDeriveColumn merge columns on 2 nodes works", {

  dssSubset('iris_der', 'iris', row.filter = '1==1')
  session1$iris_der[,c('A', 'B', 'C')] <- session2$iris_der[,1:3]
  session2$iris_der[,c('D', 'G', 'C')] <- session1$iris_der[,1:3]
  session1$iris_der[c(3,5,8,9), 'Sepal.Length'] <- NA
  session1$iris_der[c(3,6,7,9), 'Sepal.Width'] <- NA
  session2$iris_der[c(31,51,18,40), 'Sepal.Length'] <- NA
  session2$iris_der[c(1,11,21,19), 'Sepal.Width'] <- NA
  session1$iris_der[c(33,64,72,19), 'A'] <- NA
  session2$iris_der[c(34,52,28,41), 'D'] <- NA
   my_dict <- list('Sepal.Length' = c('Sepal.Width', 'Petal.Length'),
                   'A' = c('D', 'C'),
                   'G' = c('B'))

  assign('my_dict', my_dict, envir = .GlobalEnv)
  dssDeriveColumn('iris_der', 'newSepal.Length', 'mergeConceptIds("Sepal.Length", "my_dict")', datasources = opals)
  dssDeriveColumn('iris_der', 'newA', 'mergeConceptIds("A", "my_dict")', datasources = opals)
  expect_equal(session1$iris_der[3, 'newSepal.Length'], 1.3)

})
