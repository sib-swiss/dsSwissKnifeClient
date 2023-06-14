

test_that("dssMergeColumns works on 2 nodes", {

  dssSubset('iris_der', 'iris', row.filter = '1==1')
  session1$iris_der[,c('Aa', 'Bb', 'Cc', 'Ee')] <- session2$iris_der[,1:4]
  session2$iris_der[,c('Dd', 'Gg', 'Cc', 'Hh')] <- session1$iris_der[,1:4]
  session1$iris_der[c(3,5,8,9), 'Sepal.Length'] <- NA
  session1$iris_der[c(3,6,7,9), 'Sepal.Width'] <- NA
  session2$iris_der[c(31,51,18,40), 'Sepal.Length'] <- NA
  session2$iris_der[c(1,11,21,19), 'Sepal.Width'] <- NA
  session1$iris_der[c(33,64,72,19), 'Aa'] <- NA
  session2$iris_der[c(34,52,28,41), 'Dd'] <- NA
  my_dict1 <- list('Sepal.Length' = c('Sepal.Width', 'Petal.Length'),
                   'Aa' = c('Dd', 'Cc'),
                   'Gg' = c('Bb'),
                   'Ff' = c('Ee'))

  assign('my_dict1', my_dict1, envir = .GlobalEnv)
 dssMergeColumns('iris_der', 'my_dict1')
    expect_equal(session1$iris_der[3, 'Sepal.Length_derived'], 1.3)

})
