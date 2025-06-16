#' @title Call selected functions from the package mvtnorm
#' @description At the moment only the function dmvnorm is implemented
#' @param func a character, the name of the function to call
#' @param ... further arguments to be passed to the function (see the documentation of the mvtnorm package).
#' The object names present in the remote sessions must be sent as characters ('x' instead of x).
#' @param newobj, the name of the object in the remote session that will contain the result of the function. If NULL (the default), the result will not be saved in the remote session.
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#'
#' @export
#'

dssMvtnorm <- function(func, ..., newobj = NULL, async = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  func <- .encode.arg(func)
  cally <- paste0('mvtnormDSS("', func, '","', arglist, '","', newobj, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)

}
