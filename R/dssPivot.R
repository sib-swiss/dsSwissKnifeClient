#' @title Widen a dataframe using reshape2::dcast
#' @description This function  transforms long format data to wide format using the function dcast from the package reshape2.
#' @param symbol  a character name of the new dataframe in wide format
#' @param what a character name of data frame in CDISC format
#' @param value.var name of the column which stores values, this goes straight into the parameter value.var in reshape2::dcast
#' @param cols a vector of columns used in the formula. They must exist in 'what'. If null, all the columns from 'what' will be used.
#' @param formula the formula to use by reshape2::dcast. If left null a weak attempt will be made at automatically guessing it.
#' @param by.col typically the patient id, the column that will end up as key of the resulting 'wide' data.frame. In most cases this
#' can be left null as it's been set when the package was loaded
#' @param completeCases a logical, keep only complete cases?
#' @param fun.aggregate a function, see fun.aggregate in reshape2::dcast. Note: if the widening needs an aggregate function and none is provided, ds2.pivot will fail  (unlike reshape2::dcast)
#' @export
#'

dssPivot <- function(symbol, what, value.var, cols = NULL, formula = NULL, by.col = NULL, completeCases = FALSE, fun.aggregate = NULL, datasources = NULL, async = FALSE){
  # transform long to wide format (function 'wide' on the server uses reshape2::dcast)
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }

  expr <- paste0('widenDSS(', what, ',"' , value.var, '"' )
  cols.arg <- .encode.arg(cols)
  expr <- paste0(expr, ', "', cols.arg, '"')
  formula.arg <- .encode.arg(formula)
  expr <- paste0(expr, ', "', formula.arg, '"')
  by.col <- .encode.arg(by.col)
  expr <- paste0(expr, ', "', by.col, '"')
#  more.cols <- .encode.arg(more.cols)
#  expr <- paste0(expr, ', "', more.cols, '"')
  expr <- paste0(expr, ', ', completeCases , '')
#  replace.levels <- .encode.arg(replace.levels)
#  expr <- paste0(expr, ', "', replace.levels, '"')
  fun.aggregate <- .encode.arg(fun.aggregate)
  expr <- paste0(expr, ', "', fun.aggregate, '"')
  expr <- paste0(expr, ')')

  #invisible(sapply(datasources, .check.messages))

}


#' @title Suggest a formula for widening a dataframe
#' @description Makes a crude attempt at guessing an appropriate widening formula to feed into dssPivot.
#' The method is pretty basic, it should serve more as a starting point for further investigation
#' @param what a character name of the dataframe
#' @param cols a vector of columns used in the formula. They must exist in 'what'. If null, all the columns from 'what' will be used.
#' @param by.col typically the patient id, the column that will end up as key of the resulting 'wide' data.frame.
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export
#'

dssSuggestPivot <- function(what, cols = NULL, by.col = NULL,  async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }#' @param colour.pool vector of colours that will be picked sequentially.

  expr <- paste0('suggestPivotFormula(', what)
  cols.arg <- .encode.arg(cols)
  expr <- paste0(expr, ',"', cols.arg, '"')
  by.col.arg <- .encode.arg(by.col)
  expr <- paste0(expr, ', "', by.col.arg , '"', ')')


}



