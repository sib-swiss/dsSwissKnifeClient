test_that("dssRbind works", {
 dssRbind('double_iris', 'iris', 'iris', datasources = opals)
 x <- dssVar('double_iris$Sepal.Length', async = FALSE,  datasources = opals["local1"])
 expect_equal(x$global$len, 80)
})
