

#'
#' @title Representation of a Gaussian component.
#' @description Represents a set of normally-distributed points in R^n
#'     by their mean vector, covariance matrix and number of data points.
#'
.Gaussian <- setRefClass("Gaussian", fields = list(
    mu = "numeric",     # mean vector
    Sigma = "matrix",   # covariance matrix
    size = "numeric"    # sample size
))

#'
#' @title Representation of a Gaussian Mixture.
#' @description Just a wrapper on a list of Gaussian - to better understand what is in the list.
#'
.Mixture <- setRefClass("Mixture", fields = list(
    components = "list"   # list of Gaussian
))

#'
#' @title Call mixtools on each source,
#' @description
#'     Call `solve.mixtools` on all remote sources.
#' @param what: The data frame.
#' @param cols: Vector of column names for the numeric variables to consider.
#' @param K: the number of expected Gaussian components in the mix.
#'
dssGM <- function(what, cols = NULL, K, async = TRUE,  datasources = NULL) {
    if(is.null(datasources)){
        datasources <- datashield.connections_find()
    }

    expr <- paste0('gmDSS(', what)
    cols.arg <- .encode.arg(cols)
    expr <- paste0(expr, ', "', cols.arg , '"')
    K.arg <- .encode.arg(K)
    expr <- paste0(expr, ', "', K.arg , '"', ')')


    mixtures = datashield.aggregate(datasources, as.symbol(expr), async)

    return(mixtures)
}

