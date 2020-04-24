#' @title  Add and interval column to a dataframe
#' @description  Add a column containing the difference between 2 date columns. The date columns must be in the same dataframe
#' @param newobj a character, name of the new dataframe
#' @param df a character, name of the original dataframe
#' @param description.list a named list containing the name of the new colunn pointing to a list containing the start date and end date.
#' The format is list(<new column name> = list(start_date = '<existing date column>', end_date = '<existing date column>')) - see example.
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#'

dssAddDaysInterval <- function(newobj, df, description.list, async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  # example description.list:
  # list(LB_ELAPSED= list(end_date = 'LBDTC_lb2', start_date = 'MHDTC'))
  arglist <- .encode.arg(description.list)
  cally <- paste0('addDaysInterval(', df,',"', arglist, '")')
  opal::datashield.assign(datasources, symbol = newobj, value = as.symbol(cally), async = async, wait = wait)

}
