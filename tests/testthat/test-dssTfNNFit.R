test_that("dssTfNNFit works", {
  n <- dim(iris)[1]
  iris <- iris[sample(n),]

  nr_classes = nlevels(iris$Species)
  library(keras)
  model <- keras_model_sequential() %>%
    layer_dense(units = 10, activation = "relu", input_shape = 4) %>%
    layer_dropout(0.2) %>%
    #  layer_dense(5, activation = "relu") %>%
    layer_dense(nr_classes, activation = "softmax")


  json.m <- model_to_json(model)
  #
  weights <- dssTfNNFit("iris", json.m, cl.labels = "Species", list(loss = 'categorical_crossentropy', optimizer = 'optimizer_rmsprop' , metrics = 'accuracy'),
                        list(epochs = 50, batch_size = 8, validation_split = 0.2))

})



