#' @title Call selected functions from the package DDRTree
#' @description At the moment only the function 'DDRTree' is implemented. Due to implementation specifics the 'X' argument
#' for this function is mandatory (and is specified as a character, the name of a data frame).
#' @param func a character, the name of the function to call
#' @param ... further arguments to be passed to the function (see the documentation of the DDRTree package).
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#'
#' @export
#'

dssDDRTree <- function(func, ..., async = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- list(...)
  arglist <- .encode.arg(arglist)
  func <- .encode.arg(func)

  cally <- paste0('DDRTreeDSS("', func, '","', arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)

}
