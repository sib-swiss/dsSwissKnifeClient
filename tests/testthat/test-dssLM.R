
test_that("dssMergeColumns works on 2 nodes", {
 x <- dssLM('iris', 'Sepal.Length', 'Petal.Width')
 y <- lm(iris$Sepal.Length ~ iris$Petal.Width)
expect_true(all(y$coefficients - x$beta < 1e-10))
})
