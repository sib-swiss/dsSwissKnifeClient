#' @title Aggregate linear models into a single one for the estimation of beta.
#' @description Takes as input multiple results from `linregDSS` to make a single linear model,
#'    as if individuals from the multiple sources were pooled together before modeling.
#' @param what: dataframe name.
#' @param dep_var: [string] the column name for the dependent variable (y).
#' @param expl_vars: [vector of strings] the column names for the explanatory variables.
#' @param type: [string] if 'split', return a list of the individual models of each datasource, if 'combine' return the global model.
#'   If TRUE, merge them into a single model (default).
#' @return the MLE estimator for beta for the pooled individuals.
#'
dssLM <- function(what, dep_var, expl_vars = NULL, async = TRUE,  datasources = NULL, type = 'combine') {
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
    if(type == 'combine'){
      combined <- TRUE
    } else {
      combined <- FALSE
    }

    expr <- list(as.symbol('linregDSS'),  as.symbol(what),  .encode.arg(dep_var), .encode.arg(expl_vars))
    reslist <- datashield.aggregate(datasources, as.call(expr), async = async)
    if (combined) {
        result = .linreg.reduce(reslist)
        return (result)
    } else {
        result = lapply(reslist, function(r) {
            .linreg.one(r$XX, r$Xy, r$yy, r$size)
        })
        return (result)
    }
}

#'
#' @title Aggregate linear models into a single one for the estimation of beta.
#' @description Takes as input multiple results from `linregDSS` to make a single linear model,
#'    as if individuals from the multiple sources were pooled together before modeling.
#' @param reslist: a list of the results from multiple calls to `linregDSS`
#'    with the same dependent and explanatory variables.
#' @return the MLE estimator for beta for the pooled individuals.
#'
.linreg.reduce <- function(reslist) {
    XXlist = lapply(reslist, function(r) {r$XX})
    Xylist = lapply(reslist, function(r) {r$Xy})
    yylist = lapply(reslist, function(r) {r$yy})
    nlist = lapply(reslist, function(r) {r$size})
    XX = Reduce("+", XXlist)
    Xy = Reduce("+", Xylist)
    yy = Reduce("+", yylist)
    n = Reduce("+", nlist)
    result = .linreg.one(XX, Xy, yy, n)
    return(result)
}

#'
#' @title Use the quantities (X'X, X'y, y'y, sample size) sent from the individual datasources
#'   to reconstruct a complete linear model.
#'
.linreg.one <- function(XX, Xy, yy, n) {
    # Compute beta
    XXinv = solve(XX)
    b = XXinv %*% Xy
    # Compute T statistic and p-value
    SSres = as.numeric(yy - t(b) %*% Xy)
    SSres = as.numeric(yy - t(b) %*% Xy)
    k = length(b) - 1  # one is the intercept
    df = n - k - 1  # degrees of freedom
    varBeta = (SSres / df) * XXinv
    tstat = b / sqrt(diag(varBeta))  # Student T statistic
    pval = 2 * pt(abs(tstat), df, lower=FALSE)  # Student test
    result = list(
        beta = b,
        t.statistic = tstat,
        p.val = pval,
        degrees.free = df,
        SSres = SSres
    )
    return(result)
}



