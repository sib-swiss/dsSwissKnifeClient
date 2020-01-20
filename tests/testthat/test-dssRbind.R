test_that("dssRbind works", {
 dssRbind('double_iris', 'iris', 'iris', datasources = opals[1])
 x <- dssVar('double_iris$Sepal.Length', datasources = opals[1])
 expect_equal(x$global$len, 80)
})
