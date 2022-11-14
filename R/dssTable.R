#' @title Execute the R table() function remotely. Counts under the privacy level threshold will be returned as 0.
#' @param colname a character, the name of the column
#' @param type a character, should the results from different nodes be combined in one vector? Possibble values "split" or "combine"
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a table of counts
#' @export
#'
dssTable <- function(colname, type = "combine", async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  expr <- list(as.symbol('tableDSS'))
  expr$x <- .encode.arg(colname)

  res <- datashield.aggregate(datasources, as.call(expr), async = async)
  if(type == 'combine'){
    res <- list(global =Reduce(function(x,y){
      newnames <- union(names(x), names(y))
      x[setdiff(newnames, names(x))] <- 0
      y[setdiff(newnames, names(y))] <- 0
      x[sort(names(x))] + y[sort(names(y))]
    }, res))
  }
  res
}


