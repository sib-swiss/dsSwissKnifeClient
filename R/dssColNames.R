#' @title  Get or set the column names of a dataframe
#' @description Unlike ds.colnames this function allows to set the names too
#' @param df a character, name of the dataframe
#' @param value  a vector containing the new column names (if null, the function simply returns the colnames)
#' @param to.replace a vector containing the names of the columns to replace. It must have the same length as the value vector. If null, all column names will be replaced.
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return the column names of the dataframe
#' @examples
#' # open a local pseudo connection:
#' library(DSLite)
#' dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
#' dslite.server2 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
#' builder$append(server="server2", url='dslite.server2',driver = "DSLiteDriver")
#' logindata <- builder$build()
#' opals <- datashield.login(logins = logindata, assign = TRUE)
#' # load the iris dataset
#' datashield.aggregate(opals[1], as.symbol('partialData("iris", 1, 70)'))
#' datashield.aggregate(opals[2], as.symbol('partialData("iris", 71, 150)'))
#  # check the colnames:
#' dssColNames('iris')
#' # set them to something else:
#' dssColNames('iris', value =  c('sl', 'sw', 'pl', 'pw', 'sp'))
#' #check again:
#' dssColNames('iris')

dssColNames<- function(df, value =  NULL, to.replace = NULL, async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  if(!is.null(to.replace) && !is.null(value)){
    if(length(to.replace)%%length(value) != 0){
      stop('Number of items to replace is not a multiple of replacement length')
    }
  }
  #expr <- paste0('colnamesDSS(', df,',"', .encode.arg(to.replace), '", "', .encode.arg(value),'")')
 # datashield.aggregate(datasources, as.symbol(expr), async = async)
  expr <- list(as.symbol('colnamesDSS'),  as.symbol(df), .encode.arg(to.replace), .encode.arg(value))
  datashield.aggregate(datasources, as.call(expr), async = async)
}
