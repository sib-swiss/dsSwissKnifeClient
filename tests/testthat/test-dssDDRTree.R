test_that("dssDDRTree works", {

  DDRTree_res <- dssDDRTree('DDRTree', X='iris', dimensions = 2, maxIter = 5, sigma = 1e-2,
                         lambda = 1, ncenter = 3, param.gamma = 10, tol = 1e-2, datasources = opals[1])
  expect_equal(class(DDRTree_res$server1$stree)[1],"dgCMatrix")
})

