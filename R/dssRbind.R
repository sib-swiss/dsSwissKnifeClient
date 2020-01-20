#' @title  Rbind 2 dataframes
#' @param new.object a character name of the new dataframe
#' @param x,y characters names of the input dataframes.
#' @param new.colnames a vector the column names of the new dataframe (optional)
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#'

dssRbind <- function(newobj, x, y, new.colnames = NULL, async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }

  new.colnames <- .encode.arg(new.colnames)
  cally <- paste0('rbindDS(', x, ',', y, ',"', new.colnames, '")')
  opal::datashield.assign(datasources, newobj, as.symbol(cally), async = async, wait = wait)
}
