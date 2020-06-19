test_that("dssAs works", {
  dssAs('matrix', 'iris', 'iris_matrix', datasources = opals[1])
  expect_equal(class(opals$local1$envir$iris_matrix), 'matrix')
})
