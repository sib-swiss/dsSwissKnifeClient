#' @title Call selected functions from the package SNFtool
#' @description At the moment the functions 'standardNormalization','dist2','affinityMatrix' and 'spectralClustering' are implemented
#' @details The input to the function standardNormalizaton must be a dataframe containing the SUBJID colum and must *not* be transposed
#' @param func a character, the name of the function to call
#' @param symbols a character vector, name(s) of the input object(s) to the function.
#' @param ... furhter arguments which will be passed to the function (see documentation of the SDFtool package)
#' @param action a character vector, what to do with the result. 'return' doesn't work for standardNormalization as it would disclose individual data.
#' @param keep.name a character, if action is 'keep', the name of the  output object on the remote nodes. If NULL, a name will be generated.
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)

#' @export
#'

dssSNF <- function(func, symbols,..., action = c('keep', 'return'), keep.name = NULL, async = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  symbols <- .encode.arg(symbols)
  action <- .encode.arg(action)
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to  the local nodes

  expr <- paste0('snfDSS("', func, '","', symbols, '","', arglist, '","', action, '"')
  if(!is.null(keep.name)){
    expr <- paste0(expr,  ',"',keep.name, '"')
  }
  expr <- paste0(expr, ')')



}
