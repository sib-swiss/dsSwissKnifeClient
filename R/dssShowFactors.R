#' @title Describe factors in a dataframe
#' @description Show the levels for all the factors in a remote dataframe
#' @param what a character name of the dataframe
#' @param show.all a logical, default FALSE. If FALSE it limits to the first 50 levels, otherwise it returns all of them.
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export
#'

dssShowFactors <- function(what, show.all = FALSE,async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  limit.levels <- as.character(!show.all)
  expr <- paste0('showInfo(', what, ',"', limit.levels, '")')
  out <- datashield.aggregate(datasources, as.symbol(expr), async = async, wait = wait)
  return(out)

}
