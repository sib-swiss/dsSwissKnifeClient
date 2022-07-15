test_that("dssUwot works", {
 iris_umap1 <- dssUwot('umap', 'iris',async = TRUE, datasources = opals[1], n_neighbors = 5, learning_rate = 0.5, init = "random", n_epochs = 20, ret_model = TRUE)
})

