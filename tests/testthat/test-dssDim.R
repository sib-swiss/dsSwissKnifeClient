test_that("dssDim works", {
  rem <- dssDim('iris', datasources = opals)
  expect_equal(rem$local1, dim(part_iris_1))
  expect_equal(rem$local2, dim(part_iris_2))
})
