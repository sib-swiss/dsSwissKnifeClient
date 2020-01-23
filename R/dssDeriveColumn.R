#' @title Add a calculated column to a dataframe
#' @description The new column is calculated from a formula that can contain arithmetic operators and a (very) limited number of R functions.
#' At the moment the permitted functions are: abs, round, ceiling, floor, trunc, signif, length.
#' @param df  a character, the name of the dataframe
#' @param new.col a character, the name of the new column
#' @param formula a character, the formula used to calculate the new column. It can reference columns from the dataframe, or other objects in the session
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return It doesn't return anything, the dataframe in the server session will now contain the new column.
#' @examples
#' # open a local pseudo connection:
#' x <- dssCreateFakeServers('test', servers = 1, tie_first_to_GlobalEnv = 1)
#' opals <- datashield.login(x)
#' #now play around:
#' dssDeriveColumn('iris', 'new_col', 'Sepal.Length/round(2.2)', datasources = opals)
#' str(iris)
#' dssDeriveColumn('iris', 'new_col2', 'iris2$Sepal.Length/abs(-2)', datasources = opals)
#' str(iris)
#' dssDeriveColumn('iris', 'new_col3', 'a/abs(-2)', datasources = opals)
#' str(iris)
#' # this will fail:
#' dssDeriveColumn('iris', 'new_col', 'Sepal.Length[1]', datasources = opals)
#' # as will this
#' dssDeriveColumn('iris', 'new_col', 'rnorm(length(iris$Sepal.Length), mean = mean(iris$Sepal.Length),1)', datasources = opals)
#'

dssDeriveColumn <- function(df, col.name, formula, datasources = NULL, async = TRUE, wait = TRUE){
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  formula <- .encode.arg(formula)
  cally <- paste0('deriveColumn(', df,',"', col.name, '", "', formula, '")')
  datashield.assign(datasources, df, as.symbol(cally), async = async, wait = wait)

}



