dssUwot <- function(func, X, async = TRUE, datasources = NULL, ...){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  cally <- paste0('uwotDSS("', func, '",', X , ',"', arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)
}
