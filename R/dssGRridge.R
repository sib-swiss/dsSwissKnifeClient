dssGRridge <- function(func, ...,  newobj = NULL, async = TRUE, wait = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  arglist <- .encode.arg(list(...)) # pass the args list almost as is to the original function on the local nodes
  func <- .encode.arg(func)
  newobj <- .encode.arg(newobj)
  cally <- paste0('GRridgeDS2("', func, '","', arglist, '","', newobj, '")')
  opal::datashield.aggregate(datasources, as.symbol(cally), async = async, wait = wait)

}
