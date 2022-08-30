test_that("dssUpload works without chunking", {
  ir_list <- as.list(iris)
  dssUpload('ir_list', NULL, datasources = opals[1])
  expect_identical(ir_list, session1$ir_list)
  })

test_that("dssUpload works with chunking", {
  ir_list <- as.list(iris)
  dssUpload('ir_list', 10, datasources = opals[1])
  expect_identical(ir_list, session1$ir_list)
})
