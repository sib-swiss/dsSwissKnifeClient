#' @title  Remote clogit
#' @description Executes clogit \{survival\} on the remote nodes
#' @param ... arguments to be sent to clogit.
#' Note, the data argument must be a character, the name of the input dataframe.
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return  A stripped down clogit model (without the call and the residuals)
#'
#' @examples
#' # open a local pseudo connection:
#' opals <- dssCreatePseudoServers(servers = 1, tie_first_to_GlobalEnv = 1)
#' # load the infert dataset
#' datashield.aggregate(opals[1], as.symbol('partialData("infert")'))
#' clogit.model <- dssClogit(formula = case ~ spontaneous + induced + stratum, data='infert', datasources = opals[1])
#' summary(clogit.model$local1)
#'
#' @export
#'

dssClogit <- function(..., async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  arglist <- list(...) # pass the args list almost as is to clogit on the local nodes
  if(typeof(arglist$formula) ==  'language'){
    arglist$formula <- Reduce(paste, deparse(arglist$formula)) # Reduce for formulas longer than 60 chars - deparse splits them into a vector
  }
  arglist <- .encode.arg(arglist)
  cally <- paste0('clogitDSS("', arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)
}
