#' @title Generate synthetic data using the synthpop package
#' @param func a character, the name of the function to call (all functions are available).
#' @param ... further arguments to be passed to the function (see the documentation of the Synthpop package).
#' Object names should be passed as character strings.
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
  #'
#' @export
#'

dssSynthpop <- function(func, ..., async = TRUE, wait = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  func <- .encode.arg(func)
  newobj <- .encode.arg(newobj)
  cally <- paste0('synthpopDSS("', func, '","', arglist, '","', newobj, '")')
  opal::datashield.aggregate(datasources, as.symbol(cally), async = async, wait = wait)

}

