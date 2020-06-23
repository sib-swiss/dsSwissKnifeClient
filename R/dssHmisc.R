#' @title Call selected functions from the package Hmisc
#' @description At the moment only the function 'rcorrcens' is implemented. Due to implementation specifics the 'data' argument
#' for this function is mandatory (and is specified as a character, the name of a data frame) and the 'formula' argument must be set as a character (enclosed in quotes)
#' @param func a character, the name of the function to call
#' @param ... further arguments to be passed to the function (see the documentation of the Hmisc package).
#' @param newobj a character name of the new object to be created on the nodes if this is an assign function, ignored otherwise.
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#'
#' @export
#'

dssHmisc <- function(func, ...,  newobj = NULL, async = TRUE, wait = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  arglist <- list(...)
  arglist <- .encode.arg(arglist)
  func <- .encode.arg(func)
  newobj <- .encode.arg(newobj)
  cally <- paste0('HmiscDSS("', func, '","', arglist, '","', newobj, '")')
  opal::datashield.aggregate(datasources, as.symbol(cally), async = async, wait = wait)

}
