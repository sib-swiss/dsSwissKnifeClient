#' @title Call the ConsensusOPLS function from the package ConsensusOPLS
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @param ... further *named* arguments to be passed to the function (see the documentation of the ConsensusOPLS package). The names of objects (dataframes, vectors)
#' should be provided as a characters.

#' @export
#'
dssConsensusOPLS <- function(async , datasources, ...){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  cally <- paste0('consensusOPLSDSS("',arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)
}


