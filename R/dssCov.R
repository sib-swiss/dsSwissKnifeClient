#' @title The covariance matrix of a data frame or matrix
#' @param x a character name of the data frame
#' @param collist a vector of columns for the covariances. They must exist in x.
#' If null, all the *numeric* columns from x will be used.
#' @param type a character which represents the type of analysis to carry out.
#' If type is set to 'combine',  global column means are calculated if type is set to 'split', the column means are
#' calculated separately for each node.
#' @param na.rm a logical default FALSE, should NAs be removed before calculation? The TRUE value is processed only if type = 'split' and it calls the cov() function on each node with use = "pairwise.complete.obs"
#' @param wt if set, either a vector of weights or the name of such a vector. If the former, the same vector will be sent to
#' all the nodes. If the latter, different vectors can be first uploaded (with dssUpload) to the respective nodes. In both cases, the function will
#' execute cov.wt on the nodes and it will return a matrix per node irrespectve of the  the parameter "type" above
#' @param cor, center, method - parameters for the cov.wt function, ignored if wt is NULL (the default)
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a list with one element for each node (or one $global element if type='combine'). Each element contains
#' a vector with the respective column means, the covariance matrix and the number of rows
#' @export
#'


dssCov <- function(x,  collist = NULL, type = 'combine', na.rm = FALSE, wt = NULL,  cor = FALSE, center = TRUE, method = 'unbiased', async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  if(!is.null(wt)){
     expr <- list(as.symbol('partCov'), x = as.symbol(x), means = NULL, collist = collist, wt = wt, cor = cor, center = center, method = method )
     return(datashield.aggregate(datasources,as.call(expr), async=async))
  }
  mlist <- dssColMeans(x,  na.rm, collist, type, async = async, datasources)

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
  expr <- c(expr, list(na.rm = na.rm))
  #expr <- paste0(expr, ')')
  #  covs <- datashield.aggregate(datasources,as.symbol(expr), async=async)
  covs <- datashield.aggregate(datasources,as.call(expr), async=async)
  if (type == 'combine'){
    #sum them up:
    covs <- list(global = Reduce('+', covs))
    mapper <- function(x){
      list(means = mlist[[x]]$means, vcov = covs[[x]]/(mlist[[x]]$nrows - 1), nrows = mlist[[x]]$nrows )
    }
  } else {
    mapper <- function(x){
      list(means = mlist[[x]]$means, vcov = covs[[x]], nrows = mlist[[x]]$nrows )
    }
  }
  Map(mapper, names(covs))
  #Map(function(x) covs[[x]]/(mlist[[x]]$nrows - 1), names(covs) )
}
