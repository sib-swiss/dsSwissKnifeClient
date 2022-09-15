#' @title Scale a dataframe
#' @description execute base::scale() on the nodes
#' @param symbol a character name of the new, scaled dataframe
#' @param what a character name of the source dataframe
#' @param center either a logical value or a numeric vector of length equal to the number of columns of "what"
#' @param scale either a logical value or a numeric vector of length equal to the number of columns of "what"
#' @param type a character, 'split' or 'combine'. If either center or scaled are set to  TRUE it defines the
#' scope for the means used for centering and/or standard deviations used for scaling. If 'combine', global values,
#' calculated accross all nodes are used, in case of 'split' the node local values are used. Default 'combine'.

#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export
#'

dssScale <- function(symbol, what, center = TRUE, scale = TRUE, type = 'combine', async = TRUE, datasources = NULL){
  if(!(type %in% c('combine', 'split'))){
    stop('Function argument "type" has to be either "combine" or "split"')
  }
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  if(length(datasources) == 1){
    type = 'split' # regardless
  }
  if(type == 'combine'){
      useRootMeanSquare <- FALSE
      if (is.logical(center)){
      if(center){
        covs <- dssCov(what, async = async, datasources = datasources)
        center <- covs$global$means
      } else {
        useRootMeanSquare <-TRUE
      }
    }
    ctr <- .encode.arg(center)
    expr <- paste0('scaleDSS(', what, ', "', ctr, '", FALSE)')
    datashield.assign(datasources, symbol, as.symbol(expr), async = async)


    if(is.logical(scale)){
      if(scale){
        if(!useRootMeanSquare){
          covs <- dssCov(symbol, async = async, datasources = datasources) # of the centered dataframe
          scale <- sqrt(diag(covs$global$vcov))
        } else {
          # root-mean-square bother - execute partSSD with xpoint = 0
          # I don't know how many numerical columns so:
          cm <- dssColMeans(symbol, type = 'combine', async = async, datasources = datasources)$global$means

          scale <- unlist(sapply(names(cm), function(x){

            cally <- paste0('partSsd(', symbol,'$',x, ',"', .encode.arg(0), '")')
            res <- datashield.aggregate(datasources, as.symbol(cally), async = async)
            lens <- dssSwapKeys(res)[['len']]
            glob_root_mean_sq <- sum(unlist(dssSwapKeys(res)[['ssd']]))
            tot_len <- sum(unlist(lens))
            sqrt(glob_root_mean_sq/(tot_len -1))
          } ) )

        }
      }
     }
    scale <- .encode.arg(scale)
    expr <- paste0('scaleDSS(',symbol, ', FALSE ', ', "', scale, '")') # it's already centered
    datashield.assign(datasources, symbol, as.symbol(expr), async = async)

  } else {
    center <- .encode.arg(center)
    scale <- .encode.arg(scale)
    expr <- paste0('scaleDSS(',what, ',"',center, '","', scale, '")')
    datashield.assign(datasources, symbol, as.symbol(expr), async = async)
  }
}
