#' @title Mean of a vector
#' @description Same as function mean()
#' @param what  name of the vector
#' @param na.rm logical, should missing values be removed?
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return  a list containing one value per node plus one element for the global mean
#' @export

dssMean <- function (what, na.rm = TRUE, datasources = NULL) {
  # no more split/combined, return both
  if (is.null(datasources)) {
    datasources <- dsBaseClient_findLoginObjects()
  }
  expr <- paste0('partMean(', what, ', ',na.rm, ')')
  mns <- datashield.aggregate(datasources, as.symbol(expr))
  out <- Reduce(function(a,b){
      (a$mean * a$len + b$mean * b$len)/(a$len + b$len)
    }, mns)
  mns <- dssSwapKeys(mns)$mean
  mns$global <-  out

  mns
}
