#' @title Generate synthetic data using the synthpop package
#' @description The only function available is synthpop::syn
#' @param data a character, name of the data frame to use as base for the synthetic data
#' @param ... further arguments to be passed to the function (see the documentation of the synthpop package).
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
  #'
#' @export
#'

dssSynthpop <- function(data,..., async = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  data <- .encode.arg(data)
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  cally <- paste0('synthpopDSS("', data, '","',  arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)

}

