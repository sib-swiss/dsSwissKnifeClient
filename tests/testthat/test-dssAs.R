test_that("dssAs works", {
  dssAs('matrix', 'iris', 'iris_matrix', datasources = opals[1])
  expect_equal(class(session1$iris_matrix), 'matrix')
})
