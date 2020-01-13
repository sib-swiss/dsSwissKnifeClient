test_that("dssSmooth2d works", {
   x <- dssSmooth2d('iris$Sepal.Length', 'iris$Petal.Length', draw.image = TRUE, datasources = opals)
   x <- dssSmooth2d('iris$Sepal.Length', 'iris$Petal.Length', draw.image = TRUE, categories = 'iris$Species', datasources = opals)
   x <- dssSmooth2d('iris$Sepal.Length', 'iris$Petal.Length', draw.image = TRUE, categories = 'iris$Species',
                    emphasize_level = 2, datasources = opals)
   expect_equal(names(x), c('lims', 'img'))
})

