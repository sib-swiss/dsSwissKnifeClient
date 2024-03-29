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
    datasources <- datashield.connections_find()
  }
  expr <- paste0('partMean(', what, ', ',na.rm, ')')
  mns <- datashield.aggregate(datasources, as.symbol(expr))
  out <- Reduce(function(a, b) {
    if(is.null(a[['weight']])){
      a[['weight']] <- a$mean*a$len
    }
    list(weight = a$weight + b$mean*b$len, len = a$len + b$len)
  }, mns)

  mns <- dssSwapKeys(mns)$mean
  if (is.list(out)) {
    out <- out$weight/out$len
  }
  mns$global <- out
  mns
}
