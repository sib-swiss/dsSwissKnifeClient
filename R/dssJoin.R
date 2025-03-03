#' @title  Join 2 or more dataframes
#' @description Applies dplyr::xxx_join on the local nodes where xxx can be any of ('inner', 'full', 'left', 'right', 'semi', 'anti')
#' @param what a vector of names of dataframes
#' @param symbol the name of the new dataframe
#' @param by a character or a named vector, this goes straight into the 'by' parameter of dplyr::xxx_join. If nothing is supplied, 'SUBJID' is assumed.
#' As per the dplyr documentation, to join by different variables on x and y use a named vector.
#' For example, by = c("a" = "b") will match x.a to y.b.
#' @param join.type a character, the type of the join, the possible values are above. Default is 'full'
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return It doesn't return anything,  it creates a dataframe, the result of the join
#' @examples
#' # open a local pseudo connection:
#' library(DSLite)
#' dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
#' logindata <- builder$build()
#' opals <- datashield.login(logins = logindata)
#' # load the iris dataset
#' datashield.aggregate(opals[1], as.symbol('partialData("iris")'))
#' # join iris with itself just for fun:
#' dssJoin(c('iris', 'iris'), symbol = 'joined_iris', by = 'Sepal.Length', join.type = 'inner', datasources = opals)
#' session1 <- dslite.server1$getSession(dslite.server1$getSessionIds())
#' str(session1$joined_iris)
#'
#' @export
#'


dssJoin <- function(what=NULL, symbol=NULL, by = NULL, join.type = 'full', async = TRUE, datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # if not more than one input objects stop
  if(length(what) < 2){
    stop("You must provide the names of at least two objects!", call.=FALSE)
  }

  # check if the input object(s) is(are) defined in all the studies
  if(is.null(symbol)){
    symbol <- paste0('join_', paste(what, collapse='_'))
  }
  #what.arg <- .encode.arg(paste0('c(', paste(x, collapse = ','), ')'))
  what.arg <- .encode.arg(what)
  # call the server side function
  cally <-  paste0("joinDSS('", what.arg, "', '", join.type, "'")
  if(!is.null(by)){
    by <- .encode.arg(by, serialize.it = TRUE)
    cally <- paste0(cally, ",'", by,"'" )
  }
  cally <- paste0(cally, ')')

 datashield.assign(datasources, symbol, as.symbol(cally), async = async)


}
