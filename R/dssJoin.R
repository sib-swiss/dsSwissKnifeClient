#' @title  Join 2 or more dataframes
#' @description Applies dplyr::xxx_join on the local nodes where xxx can be any of ('inner', 'full', 'left', 'right', 'semi', 'anti')
#' @param what a vector of names of dataframes
#' @param symbol the name of the new dataframe
#' @param by a character or a named vector, this goes straight into the 'by' parameter of dplyr::xxx_join. If nothing is supplied, 'SUBJID' is assumed.
#' As per the dplyr documentation, to join by different variables on x and y use a named vector.
#' For example, by = c("a" = "b") will match x.a to y.b.
#' @param join.type a character, the type of the join, the possible values are above. Default is 'full'
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return It doesn't return anything,  it creates a dataframe, the result of the join
#' @examples
#' # open a local pseudo connection:
#' opals <- dssCreatePseudoServers(servers = 1, tie_first_to_GlobalEnv = 1)
#' # load iris
#' data('iris')
#' # join iris with itself just for fun:
#' dssJoin(c('iris', 'iris'), symbol = 'joined_iris', by = 'Sepal.Length', join.type = 'inner', datasources = opals)
#' str(joined_iris)
#'
#' @export
#'


dssJoin <- function(what=NULL, symbol=NULL, by = NULL, join.type = 'full', async = TRUE, wait = TRUE, datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
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
  cally <-  paste0("join('", what.arg, "', '", join.type, "'")
  if(!is.null(by)){
    by <- .encode.arg(by)
    cally <- paste0(cally, ",'", by,"'" )
  }
  cally <- paste0(cally, ')')

 datashield.assign(datasources, symbol, as.symbol(cally), async = async, wait = wait)


}
