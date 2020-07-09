#' @title Get the combined range of a (number of) vector(s) or dataframes
#' @description If one of the parameters is a dataframe, the ranges of all the numeric columns are returned (no need to subset for them)
#' @param ... characters vector or dataframe names (must be present on all the nodes specified)
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return  combined range over the specified datasources
#' @export
#'

dssRange <- function(..., type= 'combine', datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  args <- paste(..., sep = "', '")

  cally1 <- paste0("partRange('", args ,"')")

  #ranges <- unique(unlist(datashield.aggregate(datasources, as.symbol(cally1))))
  ranges <- datashield.aggregate(datasources, as.symbol(cally1))

  #ranges <- ranges[is.finite(ranges)]
  if(type == 'combine'){
    # get rid of infinities if any (they come from _EMPTY objects)
    ranges <- sapply(ranges, function(y){
      if(is.list(y[[1]])){
        y <- unlist(y, recursive = FALSE)
      }

      sapply(y, function(x){
        x[is.finite(x)]
      }, simplify = FALSE,USE.NAMES = TRUE)
    }, simplify = FALSE, USE.NAMES = TRUE)

    swapped <- dssSwapKeys(ranges)

    out <- sapply(swapped, function(x){
      all <- Reduce(c,x)
      c(min(all, na.rm = TRUE), max(all, na.rm = TRUE))
    }, simplify = FALSE, USE.NAMES = TRUE)

    ranges <- list(global = out)
  }

  return(ranges)

}
