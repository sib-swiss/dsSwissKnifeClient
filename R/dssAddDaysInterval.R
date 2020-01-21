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
#' @examples
#' # login:
#' opals <- datashield.login(logindata)
#' # load DM in colaus:
#' datashield.assign(opals['colaus'], 'dm', 'rhapsody.DM')
#' # add an interval column as the difference in days between RFSDTC and BRTHDTC:
#' ds2.add.days.interval('dm2', 'dm', list(DM_ELAPSED = list(end_date = 'RFSTDTC', start_date = 'BRTHDTC')), datasources = opals['colaus'])
#' # check the newly created column:
#'ds.summary('dm2$DM_ELAPSED', datasources = opals['colaus'])
#'
#'
#'
dssAddDaysInterval <- function(newobj, df, description.list, async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  # example description.list:
  # list(LB_ELAPSED= list(end_date = 'LBDTC_lb2', start_date = 'MHDTC'))
  arglist <- .encode.arg(description.list)
  cally <- paste0('addDaysInterval(', df,',"', arglist, '")')
  opal::datashield.assign(datasources, symbol = newobj, value = as.symbol(cally), async = async, wait = wait)

}
