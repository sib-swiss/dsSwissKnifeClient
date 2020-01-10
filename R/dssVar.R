#' @title Compute the variance of a given vector
#' @description Similar to the R function var, corrects the datashield function ds.var.
#' @param what a character, the name of a numerical vector.
#' @param type a character which represents the type of analysis to carry out.
#' If type is set to 'combine', a global variance is calculated.
#' If type is set to 'split', the variance is calculated separately for each study.
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return A list containing:
#' In the case of 'combine', a list with a 'global' element (the global variance).
#' In the case of 'split', a list with as many elements as the 'datasources' argument (the respective variances).
#' @export
#'
dssVar <- function(what, type = 'combine', datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }
  xpoint <- NULL
  if(type == 'split'){
    xpoint <- NULL
  } else if(type == 'combine'){
    xpoint <- dssMean(what, datasources = datasources)$global
  } else {
    stop('Type must be one of "split" or "combine".')
  }
  cally <- paste0('partSsd(', what, ',', xpoint, ')')
  res <- datashield.aggregate(datasources, as.symbol(cally), async = TRUE, wait = TRUE)
  if (type == 'split'){
    res <- sapply(res, function(x){
      x$ssd/(x$len - 1)
    }, simplify = FALSE)
  } else if (type == 'combine'){
    out <- sapply(dssSwapKeys(res), function(x) Reduce('+', x), simplify = FALSE)
    res <- list(global =  out$ssd/(out$len -1))
  }
  res
}
