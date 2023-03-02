
test_that("dssMergeColumns works on 2 nodes", {
 x <- dssLM('iris', 'Sepal.Length', 'Sepal.Width')
 y <- lm(iris$Sepal.Length ~ iris$Sepal.Width)
expect_true(all(y$coefficients - x$beta < 1e-10))
})
