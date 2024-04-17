
test_that("dssLM works on 2 nodes", {
 x <- dssLM('iris', 'Sepal.Length', c('Petal.Width', 'Petal.Length'))
 y <- lm(iris$Sepal.Length ~ iris$Petal.Width + iris$Petal.Length)
suppressWarnings(z <- dsBaseClient::ds.glm(iris$Sepal.Length ~ iris$Petal.Width + iris$Petal.Length, family = 'gaussian', viewVarCov = TRUE, maxit=10))
 u <- dssCov('iris', c('Petal.Width', 'Petal.Length'))
expect_true(all(y$coefficients - x$beta < 1e-10))
})
