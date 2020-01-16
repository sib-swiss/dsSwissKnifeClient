#' @title  Subset a dataframe using free filters
#' @description Alternative to the datashield function ds.subset
#' @param symbol the name of the new dataframe
#' @param what the name of the source dataframe
#' @param row.filter a character containing the row filtering conditions. The filter is executed in the dataframe environment so no need to
#' qualify the column names. Ex: .... row.filter = "LBTESTCD == 'HDL'"
#' @param col.filter a character containing the column filtering conditions. Ex: "c('LBTESTCD', 'LBORRES')"
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return It doesn't return anything,  it creates a filtered dataframe on the remote node
#' @examples
#' # open a local pseudo connection:
#' x <- dssCreateFakeServers('test', servers = 1, tie_first_to_GlobalEnv = 1)
#' opals <- datashield.login(x)
#' # load iris
#' data('iris')
#' # now play around:
#' dssSubset('iris_filtered', 'iris', row.filter = 'Sepal.Length < 6 & Species == "setosa"', col.filter = '!(colnames(iris) == "Petal.Width")', datasources = opals)
#' str(iris_filtered)
#'
#' @export
#'

dssSubset <- function(symbol, what, row.filter = TRUE, col.filter = TRUE, async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  cally <- paste0('safeSubset(', what)
  row.filter <- .encode.arg(row.filter)
  cally <- paste0(cally, ',"', row.filter, '"')
  col.filter <- .encode.arg(col.filter)
  cally <- paste0(cally, ',"', col.filter, '"')

  cally <- paste0(cally, ')')

datashield.assign(datasources, symbol = symbol, value = as.symbol(cally), async = async, wait = wait)

}
