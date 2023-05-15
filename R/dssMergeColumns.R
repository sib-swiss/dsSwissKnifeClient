#' @title Merge columns in dataframe
#' @description Combine the contents of similar fields with similar codes according to a dictionary.
#' @param dictionary a list of the form (output_col_1 = list(input_col_1,... input_col_n), .... output_col_n = list(...))
#' The input columns will be read in sequence and the existing NAs will be filled with values from the subsequent columns in the list.
#' The end result is the output column.
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return It doesn't return anything, the dataframe in the server session will now contain the new output columns.

dssMergeColumns <- function(df, dictionary, datasources = NULL, async = TRUE){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  dssUpload(dictionary, datasources = datasources, async = async)
  dictObject <- get(dictionary, envir = parent.frame())
  endCols <- names(dictObject)
  invisible(lapply(endCols, function(x){
    dssDeriveColumn(df, paste0(x, '_derived'), paste0('mergeConceptIds("', x , '", "',dictionary,'")'), async = async, datasources = datasources)
  }))

}



