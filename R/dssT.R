
#' @title Transpose a  matrix on the remote nodes.
#' @description See base::t()
#' @param symbol a character, name of the object to transpose
#' @param newobj a character, name of the new, transposed object
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)

#' @export
#'


dssT <- function(symbol, newobj = paste0(symbol, '_tr'), async = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  expr <- paste0('tDSS(', symbol, ')')
  datashield.assign(datasources, newobj, as.symbol(expr), async)


}
