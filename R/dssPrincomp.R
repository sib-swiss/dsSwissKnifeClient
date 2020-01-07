#' @title PCA on a distributed dataset
#' @description This function is similar to the R function princomp applied on the covariance matrix of the distributed dataset.
#' It has the side effect of creating a scores dataframe on each node - that can be used by subsequent calls to 'biplot'.
#' @param df a character name of the dataframe. The dataframe can contain character columns or factors in which case only the numeric columns will be considered.
#' @param type a character which represents the type of analysis to carry out.
#' If type is set to 'combine',  global column means are calculated if type is set to 'split', the column means are
#' calculated separately for each node.
#' @param center a logical, should the columns be centered? Default TRUE.
#' @param scale a logical, should the columns be scaled? Default FALSE.
#' @param scores.suffix a character. The  name of the scores dataframe will be the concatenation between df and scores.suffix.
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a list with one element for each node (or one $global element if type='combine'). Each element contains
#' a stripped down princomp object (the 'scores' element is replaced with the name of the scores dataframe on the remote nodes)
#' @export
#'




dssPrincomp <- function(df, type = 'combine', center = TRUE, scale = FALSE, scores.suffix = '_scores',
                         async = TRUE, wait = TRUE, datasources = NULL){

  if(!(type %in% c('combine', 'split'))){
    stop('Function argument "type" has to be either "combine" or "split"')
  }

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }

  covlist <- ds2.cov(df, type = type, async = async, wait = wait, datasources = datasources)

  pca.builder <- function(x){
    pca <- princomp(covmat = x$vcov)
    # to get an exact value of the loadings we need to recalculate the eigen vectors without assuming a symmetric matrix
    # then replace the loadings with the result
    # this allows correct matching of directions between the distributed and the local versions of the same dataset (e.g 'iris')
    xx <- eigen(x$vcov, symmetric = FALSE)
    pca$loadings[] <-xx$vectors[]

    pca$n.obs <- x$nrows
    pca$lam <- pca$sdev * sqrt(x$nrows) # for biplot
    pca$scores <- paste0(df, scores.suffix)
    class(pca) <- append( 'dssPrincomp', class(pca))
    pca
  }
  pcalist <- Map(pca.builder, covlist)

  scores.builder <- function(x){
    loadings <- .encode.arg(unname(pcalist[[x]]$loadings[]))
    means <- .encode.arg(unname(covlist[[x]]$means))
    sds <- .encode.arg(pcalist[[x]]$sdev)
    lam <- .encode.arg(pcalist[[x]]$lam)

    # expr <- paste0('pca.scores(',df, ',"', loadings, '",', center, ',', scale, ',', FALSE, ',"' , means, '","', sds, '","', lam, '")')
    #build expr as a list to be sent as.call

    expr <- list(as.symbol('pcaScores'), as.symbol(df), loadings, center, scale, FALSE, means, sds, lam)
    if(x == 'global'){
      nodes <- datasources
    } else {
      nodes <- datasources[x]
    }
    #opal::datashield.assign(nodes, paste0(df, scores.suffix), as.symbol(expr), async = async, wait = wait)
    #send expr as.call and not as.symbol (10 thousand char limit):
    opal::datashield.assign(nodes, paste0(df, scores.suffix), as.call(expr), async = async, wait = wait)

  }

  if(type == 'combine'){
    mappee <- c('global')
  } else {
    mappee <- names(datasources)
  }

  Map(scores.builder, mappee)
  pcalist
}
