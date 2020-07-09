#' @title Call selected functions from the package GRridge
#' @description At the moment only the functions 'CreatePartition'  and 'grridge'  are implemented
#' @param func a character, the name of the function to call
#' @param ... further arguments to be passed to the function (see the documentation of the GRridge package).
#' @param newobj a character name of the new object to be created on the nodes if this is an assign function(i.e 'CreatePartition'), ignored otherwise.
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#'
#' @export
#'

dssGRridge <- function(func, ...,  newobj = NULL, async = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  func <- .encode.arg(func)
  newobj <- .encode.arg(newobj)
  cally <- paste0('GRridgeDSS("', func, '","', arglist, '","', newobj, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)

}
