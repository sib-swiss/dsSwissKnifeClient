test_that("dssDim works", {
  rem <- dssDim('iris', datasources = opals)
  expect_equal(rem$server1, dim(session1$iris))
  expect_equal(rem$server2, dim(session2$iris))
})
