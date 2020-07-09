#' @title The covariance matrix of a data frame or matrix
#' @param x a character name of the data frame
#' @param collist a vector of columns for the covariances. They must exist in x.
#' If null, all the *numeric* columns from x will be used.
#' @param type a character which represents the type of analysis to carry out.
#' If type is set to 'combine',  global column means are calculated if type is set to 'split', the column means are
#' calculated separately for each node.
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a list with one element for each node (or one $global element if type='combine'). Each element contains
#' a vector with the respective column means, the covariance matrix and the number of rows
#' @export
#'


dssCov <- function(x,  collist = NULL, type = 'combine',  async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  mlist <- dssColMeans(x,  FALSE, collist, type, async = async, datasources)

  if (type == 'combine'){
    # if we didn't specify any columns grab now the numeric ones:
    if(is.null(collist)){
      collist <- mlist$global$numeric.cols
    }

    means <- unname(mlist$global$means[collist])

  } else if(type =='split'){
    means <- NULL
  } else {
    stop('Function argument "type" has to be either "combine" or "split"')
  }


  means.arg <- .encode.arg(means)

  expr <- list(as.symbol('partCov'), as.symbol(x), means.arg)

  # expr <- paste0('partial.cov(',x,',"', means.arg, '"')
  if(!is.null(collist) & length(collist) > 0){
    #expr <- paste0(expr, ',"', .encode.arg(collist), '"')
    expr <- c(expr, .encode.arg(collist))
  }
  #expr <- paste0(expr, ')')
  #  covs <- datashield.aggregate(datasources,as.symbol(expr), async=async)
  covs <- datashield.aggregate(datasources,as.call(expr), async=async)
  if (type == 'combine'){
    #sum them up:
    covs <- list(global = Reduce('+', covs))
  }
  mapper <- function(x){
    list(means = mlist[[x]]$means, vcov = covs[[x]]/(mlist[[x]]$nrows - 1), nrows = mlist[[x]]$nrows )
  }
  Map(mapper, names(covs))
  #Map(function(x) covs[[x]]/(mlist[[x]]$nrows - 1), names(covs) )
}
