#' @title  Calls nlme::groupedData on the provided arguments
#' @description  See the documentation of groupedData in package nlme
#' @param newobj a character, name of the new dataframe
#' @param ... further arguments to be passed to nlme:groupedData. The data argument must pe passed as a character, the name of the input dataframe
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign


dssNlme_groupedData <- function(newobj, ..., async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- list(...) # pass the args list almost as is to  the local nodes
  arglist <- sapply(arglist, function(x){
    if(typeof(x)=='language'){
     # return(Reduce(paste0, deparse(x))) # Reduce for formulas longer than 60 chars - deparse splits them into a vector
     return(deparse(x))
    } else {
      return(x)
    }
  }, simplify = FALSE)
  arglist <- sapply(arglist, function(x) paste(x, collapse = ''), simplify = FALSE)
 # if(typeof(arglist$formula) ==  'language'){
#    arglist$formula <- Reduce(paste, deparse(arglist$formula)) # Reduce for formulas longer than 60 chars - deparse splits them into a vector
#  }
  arglist <- .encode.arg(arglist)
  cally <- paste0('groupedDataDSS("', arglist, '")')
  datashield.assign(datasources, symbol = newobj, value = as.symbol(cally), async = async)

}


#' @title  Calls nlme::lme on the provided arguments
#' @description  See the documentation of lme in package nlme
#' @param newobj a character, name of the new dataframe
#' @param ... further arguments to be passed to nlme::lme. The data argument must pe passed as a character, the name of the input dataframe
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return a stripped down lmeObject (without the 'data' and 'call' elements). See the documentation for lme and lmeObject in package lme.
#' @examples
#' # open a local pseudo connection:
#' library(DSLite)
#' dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
#' logindata <- builder$build()
#' opals <- datashield.login(logins = logindata)#' # load the Orthodont dataset
#' # load the Orthodont dataset
#'  datashield.aggregate(opals[1], as.symbol('partialData("Orthodont", NULL, NULL, "nlme")'))
#'   dssNlme_groupedData(newobj = 'grouped', formula =  distance ~ age | Subject, data ='Orthodont', async = FALSE, datasources = opals[1] )
#'  lme.model <- dssNlme_lme(fixed = distance ~ age, data = 'grouped', random = ~ 1, async = FALSE, datasources = opals[1])
#' summary(lme.model[[1]])
#'
dssNlme_lme <- function(..., async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- list(...) # pass the args list almost as is to  the local nodes
  arglist <- sapply(arglist, function(x){
    if(typeof(x)=='language'){
      #return(Reduce(paste0, deparse(arglist[x]))) # Reduce for formulas longer than 60 chars - deparse splits them into a vector
      return(deparse(x))
    } else {
      return(x)
    }
  }, simplify = FALSE)
  arglist <- sapply(arglist, function(x) paste(x, collapse = ''), simplify = FALSE)

  arglist <- .encode.arg(arglist)
  cally <- paste0('nlme_lmeDSS("', arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)
}
