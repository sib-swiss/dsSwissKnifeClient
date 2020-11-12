test_that("dssRbind works", {
 dssRbind('double_iris', 'iris', 'iris')
 x <- dssVar('double_iris$Sepal.Length', async = FALSE,  datasources = opals["server1"])
 expect_equal(x$global$len, 80)
})
