test_that("dssVIM aggr works", {
  myplots <- dssVIM('aggr', newobj = NULL, async = TRUE, datasources = opals, numbers=TRUE, x='iris')
  expect_equal(myplots$server1$combinations,"0:0:0:0:0" )
})

test_that("dssVIM kNN works", {
suppressWarnings(dssVIM('kNN', newobj = 'imp_iris', async = TRUE, datasources = opals, imp_suffix='HELLO', data='iris'))
  expect_true(is.data.frame(session1$imp_iris))
})

