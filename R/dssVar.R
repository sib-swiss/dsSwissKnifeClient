#' @title Compute the variance of a given vector
#' @description Similar to the R function var, corrects the datashield function ds.var.
#' @param what a character, the name of a numerical vector.
#' @param type a character which represents the type of analysis to carry out.
#' If type is set to 'combine', a global variance is calculated.
#' If type is set to 'split', the variance is calculated separately for each study.
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return A list containing:
#' In the case of 'combine', a list with a 'global' element (the global variance).
#' In the case of 'split', a list with as many elements as the 'datasources' argument (the respective variances).
#' @export
#'
dssVar <- function(what, type = 'combine', async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  xpoint <- NULL
  if(type == 'split'){
    xpoint <- NULL
  } else if(type == 'combine'){
    xpoint <- dssMean(what, datasources = datasources)$global$mean
  } else {
    stop('Type must be one of "split" or "combine".')
  }
  cally <- paste0('partSsd(', what, ',"', .encode.arg(xpoint), '")')
  res <- datashield.aggregate(datasources, as.symbol(cally), async = TRUE, wait = TRUE)
  lens <- dssSwapKeys(res)[['len']]
  ret <- list()
  if (type == 'split'){
   vars <- sapply(res, function(x){
      x$ssd/(x$len - 1)
    }, simplify = FALSE)


   ret <- dssSwapKeys(list(var = vars, len = lens))
  } else if (type == 'combine'){
    glob_ssd <- sum(unlist(dssSwapKeys(res)[['ssd']]))
    tot_len <- sum(unlist(lens))
    ret <- list(global = list(var = glob_ssd/(tot_len -1), len = tot_len))
  }
  ret
}
