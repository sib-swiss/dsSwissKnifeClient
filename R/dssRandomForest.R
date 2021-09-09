#' @title Distributed random forests based on the randomForest package.
#' @description Builds a random forest on each server node,
#'   as a model to predict classification of new data of the same type
#'   (new patients, same variables). The randomForest package needs to be
#'   installed on all nodes and on the client.
#' @return a list of randomForest objects if called for training or of prediction vectors if called for testing (validation).
#' @param train a list of parameters for the training phase. The elements are: what - name of the training data frame on the server.
#' dep_var [string] - name of the response factor ("y"), i.e. the categories, expl_vars [vector[string]]  - the classification variables,
#' ... - further arguments that will be passed to the randomForest function
#' @param test a list of parameters for the validation phase. The elements are: forest [list] - a list of forests obtained in the training phase ,
#' testData: new data to classify using the forests. If testData is a character, this will be considered the name of the remote data frame;
#' the testing phase will take place on the remote servers. If testData is a local data frame the testing phase and prediction will take place in the client session.
#'  testData must have at least the columns in `expl_vars` (We want to predict the value of `dep_var` for it.)
#'
#'

dssRandomForest <- function(train = list(what = NULL, dep_var = NULL, expl_vars = NULL, ...),
                            test = list(forest = NULL, testData = NULL),
                            async = TRUE,  datasources = NULL) {
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }


  expr <- list(as.symbol('forestDSS'))
  if(length(train[!sapply(train,is.null)]) > 0 ){  # meaning at least one non null
    if (is.null(train[[2]])) {
      stop('Unsupervised version is not implemented yet')
    }
    if (is.null(train[[1]])) {
      stop('No training dataset provided')
    }
    result_type <-  'forest'
    expr$train <- .encode.arg(train)
  } else if (length(test[!sapply(test,is.null)]) == 2){ # one or the other, not both
    result_type <- 'prediction'
    #test$forest <- sapply(test$forest, unclass, simplify = FALSE) # can't jsonize otherwise
    if(is.data.frame(test[[2]])){ # local data
      return(do.call(.predict, test))
    }
    #expr$test <- .encode.arg(test, serialize.it = TRUE)
    expr$testData <- test$testData
    forests <- .encode.arg(test$forest, serialize.it = TRUE)
    if(nchar(forests) > 100000){
      expr <- c(expr, .splitInEqualChunks(forests,65535)) # there's a 64 k limit on the pattern
    } else {
      expr <- c(expr, forests)
    }
  } else {
    stop('One of the "train" or "test" lists must be populated, the latter with both elements')
  }


  # Get a list of randomForests from the nodes
  reslist <- datashield.aggregate(datasources, as.call(expr), async)
  return(reslist)
}



#'
#' @title Predict classification of new patients
#'   using the forests of our different data sources.
#' @description For classification, we sum the votes from all the forests,
#    and return the class with the most votes overall.
#    For regression, we average the estimated values from all the forests.
#' @param forests: a list of randomForest objects.
#' @param testData: [data frame] new data to classify using the forests.
#' It must have at least the columns in `expl_vars`.
#' (We want to predict the value of `dep_var` for it.)
#' @return a vector of length `nrow(testData)`.
#'
.predict <- function(forests, testData) {
  nforests = length(forests)
  predictionType = forests[[1]]$type

  # For classification, we sum the votes from all the forests,
  # and return the class with the most votes overall.
  if (predictionType == "classification") {
    # Forests may end up with different classes. Take the union of them all here.
    classes = Reduce(function(cls, forest) union(cls, forest$classes), forests, NULL)
    # Init value for the Reduce
    p0 = matrix(0, nrow(testData), length(classes))
    colnames(p0) = classes
    # Sum the votes
    sumVotes = Reduce(function(p1, forest) {
      p2 = p0
      p2[, forest$classes] = predict(forest, testData, type="vote") * forest$ntree
      # The result is a matrix with nrow(testData) rows and length(forest$classes) columns.
      # It should contain vote counts (ints), but there is a bug in this lib and it returns the
      #  fraction of votes, so we need to multiply by the number of trees.
      #  Forests may have different number of trees, depending on dataset size.
      return(p1 + p2)
    }, forests, p0)
    prediction = apply(sumVotes, 1, function(row) {
      maxclass = classes[which(row == max(row))]
      return(maxclass)
    })

    # For regression, we average the estimated values from all the forests.
  } else if (predictionType == "regression") {
    p0 = rep(0, nrow(testData))
    sumEstimations = Reduce(function(p1, forest) {
      p2 = predict(forest, testData, type="response")
      # The result is a vector with nrow(testData) elements.
      return(p1 + p2)
    }, forests, p0)
    prediction = sumEstimations / nforests
  }

  return(prediction)
}


#'
#' #deprecated
#' @title Measure the quality of a randomForest object.
#' @description The quality is the average OOB errors in the case of classification,
#'   or the average pseudo R-squared for regression.
#'   We do it in the client so that the server method can return a pure
#'   randomForest object (we avoid using a custom object between the two).
#'
.quality <- function(forestObject) {
  # TO ADD IN THE DOC IF I DECIDE TO USE THIS:
  #"Votes" from forests of different
  #   nodes are averaged, weighted by the "quality" of each forest.
  #   The quality is the average OOB errors in the case of classification,
  #   or the average pseudo R-squared for regression.
  err = 0
  if (forestObject$type == "classification") {
    # average of out-of-bag errors across all trees of the forest
    err = 1 / colMeans(forestObject$err.rate)[1]
  } else if (predictionType == "regression") {
    # average of "pseudo R-squared" across all trees of the forest
    err = 1 / mean(forestObject$rsq)
  }
  return(err)
}


