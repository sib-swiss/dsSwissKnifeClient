#' @title Column means of a data frame
#' @description This function is similar to the R function colMeans
#' @param x a character name of the data frame
#' @param na.rm a logical. Should missing values (including NaN) be omitted from the calculations?
#' @param collist a vector of columns whose means will be calculated. They must exist in x.
#' If null, all the *numeric* columns from x will be used.
#' @param type a character which represents the type of analysis to carry out.
#' If type is set to 'combine',  global column means are calculated if type is set to 'split', the column means are
#' calculated separately for each node.
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a list with one element for each node (and one $global element if type='combine'). Each element contains, for each
#' numeric column, the respective mean, number of rows and number of NAs
#' @export
#'

dssColMeans <- function(x=NULL, na.rm = TRUE, collist = NULL, type='combine', async = TRUE, wait = TRUE,  datasources=NULL){

  #adapted from ds.mean

  if(is.null(datasources)){
    datasources <- dsBaseClient:::findLoginObjects()
  }

  if(is.null(x)){
    stop("Please provide the name of the input dataframe!", call.=FALSE)
  }

  expr <- list(as.symbol('partColMeans'), as.symbol(x),na.rm)


  #cally <- paste0("partial.colMeans(", x, ", ", na.rm)
  if(!is.null(collist)){
    # cally <- paste0(cally, ',"', .encode.arg(collist), '"')
    expr <- c(expr, .encode.arg(collist))
  }
  # cally <- paste0(cally,")")
  #  measures <- opal::datashield.aggregate(datasources, as.symbol(cally), async = async, wait = wait)
  measures <- opal::datashield.aggregate(datasources, as.call(expr), async = async, wait = wait)
  measures <- sapply(measures, function(x){
    if(length(x) <= 1 && is.na(x)){
      return(NULL)
    }
    x
  }, simplify = FALSE)


  if(type == 'combine'){
    reducer <- function(x,y){
      # we want to end up with an intersection of the numeric cols from all servers

      if(is.null(x$numeric.cols)){
        numeric.cols <- y$numeric.cols
      } else {
        numeric.cols <- intersect(x$numeric.cols, y$numeric.cols)
      }

      list(
        nrows = x$nrows + y$nrows,
        means =((x$means * (x$nrows - x$nas) + y$means * (y$nrows - y$nas))/(x$nrows - x$nas + y$nrows - y$nas))[numeric.cols],
        nas = (x$nas + y$nas)[numeric.cols],
        numeric.cols = numeric.cols
      )

    }
    #global <- Reduce(reducer, measures, init = list(nrows = 0, nas = 0, means = 0, numeric.cols = NULL))
    global <- Reduce(reducer, measures)
    #global$means <- global$means/(global$nrows - global$nas)
    global$nrows <- unname(global$nrows[1])
    measures$global <- global
  } else if (type != 'split'){
    stop('Function argument "type" has to be either "combine" or "split"')
  }
  measures
}
