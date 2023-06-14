#' @title Call selected functions from the package ranger
#' @description At the moment only the functions 'ranger' (which returns a result that can be visualised) and 'kNN' (which doesn't) are implemented
#' @param func a character, the name of the function to call
#' @param newobj a character name of the new object to be created on the nodes if this is an assign function(i.e 'kNN'), ignored otherwise.
#' If an assign function is called without this, the original object will be modified in place.
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @param ... further arguments to be passed to the function (see the documentation of the VIM package). The first argument (typically a dataframe)
#' should be provided as a character.

#' @export
#'
dssRanger <- function(func, newobj = NULL, async = TRUE, datasources = NULL, ...){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  func <- .encode.arg(func)
  newobj <- .encode.arg(newobj)
  cally <- paste0('rangerDSS("', func, '","', arglist, '","', newobj, '")')

  datashield.aggregate(datasources, as.symbol(cally), async = async)
}


