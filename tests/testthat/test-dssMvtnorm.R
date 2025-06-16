test_that("dssMvtnorm works", {

y <- c(0,0)
m <- c(1,1)
dssUpload('y', datasources = opals['server1'])
dssUpload('m', datasources = opals['server1'])
  n <- dssMvtnorm('dmvnorm', 'y', 'm', newobj = 'new' ,datasources = opals['server1'])

  expect_true(is.numeric(n$server1))

})

