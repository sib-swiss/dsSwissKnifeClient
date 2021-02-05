#' @title Train neural net on all clients
#' @description Defines order of clients and trains neural net on first client, retrieves the weights and uses these weights as starting
#' point to train neural net on next client. It iterates this procedure over all clients.
#' @param x string of a dataframe name
#' @param model json string of tensorflow/keras neural network model
#' @param cl.labels string with name of class variable
#' @param compile.args list with arguments passed to compile function
#' @param fit.args list with arguments passed to fit function
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return  weights of trained neural network
#' @export

dssTfNNFit <- function (x, model, cl.labels, compile.args, fit.args, datasources = NULL ) {
  # no more split/combined, return both
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }


  callList <- list("tfNNFit", x, model, cl.labels)
  callList$compile.args <- .encode.arg(compile.args)
  callList$fit.args <- .encode.arg(fit.args)
  callList$weights.args <- NULL
  weights <- datashield.aggregate(datasources,as.call(callList))

  return(weights)
}

nn <- function() {
  #devtools::install_github("rstudio/tensorflow")
  #devtools::install_github("rstudio/keras")

  #tensorflow::install_tensorflow()
  #tensorflow::tf_config()

  library(keras)
  library(dsSwissKnife)

  data("iris")

  n <- dim(iris)[1]
  iris <- iris[sample(n),]

  flat_labels = to_categorical(as.integer(iris$Species)-1, num_classes = 3)
  nr_classes = nlevels(iris$Species)

  model <- keras_model_sequential() %>%
    layer_dense(units = 10, activation = "relu", input_shape = 4) %>%
    layer_dropout(0.2) %>%
    #  layer_dense(5, activation = "relu") %>%
    layer_dense(nr_classes, activation = "softmax")

  json.m <- model_to_json(model)
  #
  weights <- tfNNFit("iris", json.m, "Species", list(loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop(), metrics = 'accuracy'),
          list(epochs = 50, batch_size = 8, validation_split = 0.2), weights.args = NULL)

  weights.1 <- weights

  weights.2 <- tfNNFit("iris", json.m, "Species", list(loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop(), metrics = 'accuracy'),
                     list(epochs = 50, batch_size = 8, validation_split = 0.2), weights.args = weights.1)

  compile(model, loss = 'categorical_crossentropy', optimizer = optimizer_rmsprop(), metrics = 'accuracy')

  history = fit(model, as.matrix(subset(iris, select=-c(Species))), flat_labels, epochs = 50, batch_size = 8,
                validation_split = 0.2)

  weights <- get_weights(model)

  labels_pred=predict_classes(model, as.matrix(subset(iris, select=-c(Species))))

  save_model_tf(object = model, filepath = "dss_model_test")
  reloaded_model <- load_model_hdf5(filepath = "dss_model_hdf5")
  #save_model_weights_hdf5(model, filepath = "dss_weights_hdf5", overwrite = TRUE)
  #model_to_json(object)

  load_model_hdf5(model,filepath = "dss_model_hdf5")
  load_model_weights_hdf5(model,filepath = "dss_weights_hdf5")
  reloaded_model <- load_model_tf("dss_model_test")

  history = fit(reloaded_model, as.matrix(subset(iris, select=-c(Species))), flat_labels, epochs = 50, batch_size = 8,
                validation_split = 0.2)

}



