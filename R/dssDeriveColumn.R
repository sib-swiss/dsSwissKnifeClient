#' @title Add a calculated column to a dataframe
#' @description The new column is calculated from a formula that can contain arithmetic operators and a (very) limited number of R functions.
#' At the moment the permitted functions are: abs, round, ceiling, floor, trunc, signif, paste0, length, as.Date, as.character, as.numeric,
#' as.factor, gsub, grep,grepl, sqrt.
#' The administrators of remote nodes can make more functions available with a procedure described
#' in the help of the function .init in the package dsSwissKnife (?dsSwissKnife:::.init)
#' There are  two  more functions available: 1) one.versus.others(col, positive.level) - this function reduces any factor to 2 levels
#' in preparation for a logistic regression (see example below). 2) rnorm.0.1() creates a standard normal distribution in a new column.
#' 3) nuke.outliers(col, maxval, minval) replaces all values in col that ar greater than maxval or smaller than minval with NAs
#' @param df  a character, the name of the dataframe
#' @param col.name a character, the name of the new column
#' @param formula a character, the formula used to calculate the new column. It can reference columns from the dataframe, or other objects in the session
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return It doesn't return anything, the dataframe in the server session will now contain the new column.
#' @examples
#' # open a local pseudo connection:
#' library(DSLite)
#' dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
#' builder <- newDSLoginBuilder()
#' builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
#' logindata <- builder$build()
#' opals <- datashield.login(logins = logindata, assign = TRUE)
#' # load the iris dataset
#' datashield.aggregate(opals[1], as.symbol('partialData("iris")'))
#' #now play around:
#' dssDeriveColumn('iris', 'new_col', 'Sepal.Length/round(2.2)', datasources = opals)
#' str(pseudo_server$iris)
#' dssDeriveColumn('iris', 'new_col2', 'iris$Sepal.Length/abs(-2)', datasources = opals)
#' str(pseudo_server$iris)
#' a <- iris$Petal.Length
#' dssDeriveColumn('iris', 'new_col3', 'a/abs(-2)', datasources = opals)
#' str(pseudo_server$iris)
#' The built in function one.versus.others:
#' dssDeriveColumn('iris', 'new_col', 'one.versus.others(Species, "setosa")')
#' str(pseudo_server$iris$new_col)
#' #### ...Factor w/ 2 levels "setosa","no_setosa": 1 1 1 1 1 1 1 1 1 1 ...#
#' # this will fail:
#' dssDeriveColumn('iris', 'new_col', 'Sepal.Length[1]', datasources = opals)
#' # as will this
#' dssDeriveColumn('iris', 'new_col', 'rnorm(length(iris$Sepal.Length), mean = mean(iris$Sepal.Length),1)', datasources = opals)
#'

dssDeriveColumn <- function(df, col.name, formula, datasources = NULL, async = TRUE){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  formula <- .encode.arg(formula)
  cally <- paste0('deriveColumn(', df,',"', col.name, '", "', formula, '")')
  datashield.assign(datasources, df, as.symbol(cally), async = async)

}



