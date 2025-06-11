#' @title Remove outliers from all or some of the columns of a data frame and replace them with NAs.
#' @param x a character name of the data frame
#' @param collist a vector of columns. They must exist in x and be numeric.
#' If null, all the *numeric* columns from x will be used.
#' @param sigmas an integer, how many standard deviations will be kept on either side of the mean?
#' If NULL, minval and maxval will be taken into account.
#' @param minval either a number or a vector of numbers of length equal to collist (or to the number of numeric columns in x if the latter is null).
#' It sets the lower cutoff, values smaller than this will be replaced with NAs. Ignored if sigmas is set.
#' @param maxval either a number or a vector of numbers of length equal to collist (or to the number of numeric columns in x if the latter is null).
#' It sets the higher cutoff, values larger than this will be replaced with NAs. Ignored if sigmas is set.
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export


dssRemoveOutliers <- function(x, collist = NULL, sigmas = NULL, minval = NULL, maxval = NULL , async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  expr <- list(as.symbol('removeOutliersDSS'), symbol = as.symbol(x),  collist = collist, sigmas = sigmas, minval = minval, maxval = maxval)
 datashield.assign(datasources,x, as.call(expr), async=async)

}
