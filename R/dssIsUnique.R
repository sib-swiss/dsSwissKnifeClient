#' @title Check if a vector is unique (tipically a dataframe column)
#' @param what a character name of the vector (or dataframe column specified with '$' - 'df$col')
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export
#'
dssIsUnique <- function(what, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  expr <- paste0('isUnique(', what,')')
 datashield.aggregate(datasources, as.symbol(expr), async = TRUE, wait = TRUE)

}
