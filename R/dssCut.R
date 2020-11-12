#' @title Convert numeric to factor
#' @description Same  as base::cut
#' @param x	 a character name of  a numeric vector which is to be converted to a factor by cutting
#' @param new.name a character name of the new vector (or dataframe column). This parameter is ignored
#' if in.place = TRUE (see below)
#' @param df a character name of a dataframe. If specified, x and new.name above must be among its columns
#' @param in.place a boolean. If true, the numeric vector will be replaced with the output of the function
#' @param breaks either a numeric vector of two or more unique cut points or a single number
#' (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#' Use the vector form (2 or more specific cut points) in order to achieve consistent levels among
#' multiple nodes.
#' @param labels same as for the 'cut' function. If only labels and no breaks are specified, breaks will be
#' calculated as length(labels)
#' @param ... further parameters are sent directly to the cut function on each node
#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return name of the new object
#' @export
#'
dssCut <- function(x, new.name = 'newObj', df = NULL, in.place = FALSE, breaks =  NULL, labels = NULL, ...,  async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()

  }



  actual.args <- as.list(match.call())[-1]
  arglist <- RCurl::merge.list(actual.args,formals())

  if (is.null(breaks)){
    if(is.null(labels)){
      stop('Please provide at least one of breaks or labels')
    } else {
      arglist$breaks <- length(labels)

    }
  }
  # trim the arglist
  arglist$datasources = NULL
  arglist$async = NULL
  if(arglist$... == ''){
    arglist$... <- NULL
  }

  #capture the environment where to eval the args (not exceedingly nice but it'll do )
  pf <- parent.frame()
  arglist <- sapply(arglist, function(x)eval(x, envir=pf), USE.NAMES = TRUE, simplify = FALSE)
  arglist <- .encode.arg(arglist)
  cally <- paste0('cutDSS("', arglist, '")')
  datashield.aggregate(datasources, as.symbol(cally), async = async)
}
