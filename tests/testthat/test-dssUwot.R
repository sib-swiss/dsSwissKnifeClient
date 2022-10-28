test_that("remote umap works", {
 iris_umap <- dssUwot('umap', 'iris',async = TRUE, datasources = opals[1], n_neighbors = 5, learning_rate = 0.5, init = "random", n_epochs = 20, ret_model = TRUE, ret_extra = 'fgraph')
 expect_equal(names(iris_umap$server1$metric), 'euclidean')
})

test_that("remote umap transform works with a model from the local session", {

  iris_umap1 <- dssUwot('umap', 'iris',async = TRUE, datasources = opals[1], n_neighbors = 5, learning_rate = 0.5, init = "pca", n_epochs = 20, ret_model = TRUE)

  iris_umap2 <- dssUwot('umap_transform', 'iris', model = iris_umap1$server1, async = TRUE, datasources = opals[2])
 expect_equal(length(iris_umap2$server2), 220)
})
