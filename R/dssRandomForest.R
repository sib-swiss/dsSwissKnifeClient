#' @title Distributed random forests based on the randomForest package.
#' @description Builds a random forest on each server node,
#'   as a model to predict classification of new data of the same type
#'   (new patients, same variables). The randomForest package needs to be
#'   installed on all nodes and on the client.
#' @return a randomForest object.
#' @param what: name of the training data frame on the server.
#' @param dep_var: [string] the response factor ("y"), i.e. the categories
#'   that will be the leaves of each tree.
#' @param expl_vars: [vector[string]] the classification variables.
#' @param testData: [data frame] new data to classify using the forests.
#'   It must have at least the columns in `expl_vars`.
#'   (We want to predict the value of `dep_var` for it.)
#' @return a list with members `$forests`: the individual forests from the nodes,
#'   and `$prediction`, the average prediction of `testData` (if given) by all nodes together.
#'
dssRandomForest <- function(what, dep_var, expl_vars = NULL, testData = NULL,
                       async = TRUE, wait = TRUE, datasources = NULL) {
  if (is.null(datasources)) {
    datasources <- dsBaseClient:::findLoginObjects()
  }
  expr <- paste0('forestDSS(', what)
  dep_var.arg <- .encode.arg(dep_var)
  expr <- paste0(expr, ', "', dep_var.arg, '"')
  expl_vars.arg <- .encode.arg(expl_vars)
  expr <- paste0(expr, ', "', expl_vars.arg , '"', ')')
  # Get a list of randomForests from the nodes
  reslist <- opal::datashield.aggregate(datasources, as.symbol(expr), async, wait)

  result <- list(forests = reslist)
  if (!is.null(testData)) {
    testData <- testData[,expl_vars]
    result$prediction <- .predict(reslist, testData)
  }
  return(result)
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



