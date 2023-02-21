#' @title  Transform the columns of a data frame into factors
#' @description It only affects the non numeric columns (text or date)
#' @param df a character, name of the dataframe
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return nothing - the data frame is now transormed in the remote sessions


dssMakeFactors<- function(df, value =  NULL, to.replace = NULL, async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  expr <- paste0('makeFactorsDSS(', df,')')
  datashield.assign(datasources, df, as.symbol(expr), async = async)
}
