test_that("dssAs works", {
  dssAs('matrix', 'iris', 'iris_matrix', datasources = opals[1])
  expect_true('matrix' %in% class(session1$iris_matrix))
})
