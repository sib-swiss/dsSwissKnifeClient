test_that("dssGM works", {

  mixture_model <- dssGM('iris', c('Sepal.Width', 'Sepal.Length'), 2, datasources = opals)
  expect_equal(class(DDRTree_res$server1$stree)[1],"dgCMatrix")
})

