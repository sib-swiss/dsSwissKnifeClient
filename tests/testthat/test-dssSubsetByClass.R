test_that("dssSubsetByClass works", {
  play_env <- session2
  assign('a', play_env$iris$Petal.Length, envir = play_env)
  assign('b', factor(c(rep('category1',10), rep('category2',100))), envir = play_env)
  dssSubsetByClass('iris', variables = 'iris$Species',  datasources = opals['server2'])
  expect_equal(sapply(play_env$subClasses, nrow), c(setosa = 10, versicolor = 50, virginica =50))
  dssSubsetByClass('iris', variables = c('iris$Species', 'b'), subsets = 'with_b', datasources =  opals['server2'])
  expect_equal(sapply(play_env$with_b, nrow), c(setosa.category1 = 10, versicolor.category2 = 50, virginica.category2 =50))
  dssSubsetByClass(c('iris', 'a'), variables = c('iris$Species', 'b'), subsets = 'with_ab',
                   keep.cols = c('Sepal.Length', 'Sepal.Width', 'a'), datasources = opals['server2'])
  expect_equal(mean(play_env$with_ab$setosa.category1$a), 1.47)
  dssSubsetByClass('a', variables = 'b',subsets = 'a_and_b',keep.cols = c('a'), datasources = opals['server2'])
  expect_equal(sapply(play_env$a_and_b, function(x) mean(x$a)), c(category1=1.47, category2 = 4.906))
})

