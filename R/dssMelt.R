dssMelt <- function(newObj, ..., async = TRUE, datasources = NULL){
 if(is.null(datasources)){
  datasources <- datashield.connections_find()
 }
 argList <- list(...)
 expr <- c(as.symbol('meltDSS'), sapply(argList, .encode.arg, TRUE, simplify = FALSE))
 datashield.assign(datasources, newObj, as.call(expr), async)
}
