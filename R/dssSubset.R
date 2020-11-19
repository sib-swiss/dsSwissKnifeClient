#' @title  Subset a dataframe using free filters
#' @description Alternative to the datashield function ds.subset
#' @param symbol the name of the new dataframe
#' @param what the name of the source dataframe
#' @param row.filter a character containing the row filtering conditions. The filter is executed in the dataframe environment so no need to
#' qualify the column names. Ex: .... row.filter = "LBTESTCD == 'HDL'"
#' @param col.filter a character containing the column filtering conditions. Ex: "c('LBTESTCD', 'LBORRES')"
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return It doesn't return anything,  it creates a filtered dataframe on the remote node
#' @examples
#' # open a local pseudo connection:
#' library(DSLite)
#' dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
#' logindata <- builder$build()
#' opals <- datashield.login(logins = logindata)
#' # load iris
#' datashield.aggregate(opals[1], as.symbol('partialData("iris")'))
#' # put a label on the pseudo-remote session so we can retrieve it later:
#' session1 <- dslite.server1$getSession(dslite.server1$getSessionIds())
#' # now play around:
#' dssSubset('iris_filtered', 'iris', row.filter = 'Sepal.Length < 6 & Species == "setosa"', col.filter = '!(colnames(iris) == "Petal.Width")', datasources = opals)
#' str(session1$iris_filtered)
#'
#' @export
#'

dssSubset <- function(symbol, what, row.filter = TRUE, col.filter = TRUE, async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  cally <- paste0('safeSubset(', what)
  row.filter <- .encode.arg(row.filter)
  cally <- paste0(cally, ',"', row.filter, '"')
  col.filter <- .encode.arg(col.filter)
  cally <- paste0(cally, ',"', col.filter, '"')

  cally <- paste0(cally, ')')

datashield.assign(datasources, symbol = symbol, value = as.symbol(cally), async = async)

}
