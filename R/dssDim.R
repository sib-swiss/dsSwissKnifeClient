#' @title Dimension of an object
#' @description Execute base::dim on the remote nodes
#' @param x a character name of the data frame or matrix
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a list with one element for each node (and one $global element if type='combine'). Each element contains the dimensions of the object on the respective node.
#'

dssDim <- function(x=NULL, async = TRUE,  datasources=NULL){

  #adapted from ds.mean

  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }

  if(is.null(x)){
    stop("Please provide the name of the input dataframe!", call.=FALSE)
  }

  expr <- list(as.symbol('dimDSS'), as.symbol(x))
  datashield.aggregate(datasources, as.call(expr), async = async)

}
