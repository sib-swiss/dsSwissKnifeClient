#' @title Scale a dataframe
#' @description execute base::scale() on the nodes
#' @param symbol a character name of the new, scaled dataframe
#' @param what a character name of the source dataframe
#' @param center either a logical value or a numeric vector of length equal to the number of columns of "what"
#' @param scale either a logical value or a numeric vector of length equal to the number of columns of "what"
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export
#'

dssScale <- function(symbol, what, center = TRUE, scale = TRUE, async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  center <- .encode.arg(center)
  scale <- .encode.arg(scale)
  expr <- paste0('scaleDSS(', what, ', "', center, '", "', scale, '")')
  datashield.assign(datasources, symbol, as.symbol(expr), async = async)

}
