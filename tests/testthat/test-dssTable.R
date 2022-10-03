test_that("dssTable split works", {
 tb <- dssTable('iris$Species', type = 'split',async = TRUE, datasources = opals)
 expect_equal(unname(tb$server1['versicolor']), 25)
})

test_that("dssTable combine works", {
  tb <- dssTable('iris$Species', type = 'combine',async = TRUE, datasources = opals)
  expect_equal(unclass(unname((tb$global['versicolor']))), 50)
})
