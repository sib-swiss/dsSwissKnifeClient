#' @title  Subset any number of vectors and/or dataframes by the levels of a factor
#' @description Alternative to the datashield function ds.SubsetByClass
#' @param x a vector containing one or more names of vectors or dataframes present on the remote nodes
#' @param subsets a character, name of the new list containing the subsets - default "subClasses"
#' @param variables a vector containing one or more names of dataframe columns or standalone factors by which to subset
#' @param keep.cols a vector containing the names of the columns or vectors to keep in the result (by default all are kept)
#' @param async same as in datashield.assign

#' @param datasources same as in datashield.assign
#' @return It doesn't return anything of value, it creates a list containing dataframes (as many as the combinations of categories of "variables") on each node
#' @examples
#' # open a local pseudo connection:
#' opals <-  dssCreatePseudoServers('test', servers = 1)
#' # load iris and create a vector and a factor:
#' datashield.aggregate(opals[1], as.symbol('partialData("iris", 41, 150)'))
#' play_env <- opals$local1$envir
#' assign('a', play_env$iris$Petal.Length, envir = play_env)
#' assign('b', factor(c(rep('category1',10), rep('category2',100))), envir = play_env)
#' #now play around:
#' dssSubsetByClass('iris', variables = 'iris$Species', datasources = opals)
#' str(play_env$subClasses)
#' dssSubsetByClass('iris', variables = c('iris$Species', 'b'), datasources = opals)
#' str(play_env$subClasses)
#' dssSubsetByClass(c('iris', 'a'), variables = c('iris$Species', 'b'), keep.cols = c('Sepal.Length', 'Sepal.Width', 'a'), datasources = opals)
#' str(play_env$subClasses)
#' dssSubsetByClass('a', variables = 'b', keep.cols = c('a'), datasources = opals)
#' str(play_env$subClasses)
#'
#' @export
#'

dssSubsetByClass <- function (x , subsets = "subClasses", variables = NULL, keep.cols = NULL, async = TRUE,
                               datasources = NULL)
{
  if (is.null(datasources)) {
    datasources <- dsBaseClient_findLoginObjects()
  }

  x <- .encode.arg(x)
  variables <- .encode.arg(variables)
  cally <- paste0("subsetByClass('", x, "', '", variables, "'")
  if(!is.null(keep.cols)){
    cally <- paste0(cally, ", c('", paste(keep.cols, collapse = "','"), "')")
  }
  cally <- paste0(cally, ")")

  datashield.assign(datasources, subsets, as.symbol(cally), async = async)
  #finalcheck <- dsBaseClient:::isAssigned(datasources, subsets)
}
