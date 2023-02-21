#' @title Calculate residuals for a linear regression
#' @description  Create a dataframe on the remote node(s) containing the residuals for one or more linear regression models.
#' The models can be local to the respective nodes (resulted from a call to lm()) or global (resulted from a call to ds.glm).
#' The choice between the 2 behaviours is dictated by the nature of the first argument, see below
#' @param outcomes can be a vector of column names or a named list. If it is a vector, a local linear model (lm()) will be created
#' on the remote servers for each of its members using the variables in 'indvars' as predictors. If it is a named list, the
#' names must be again column names and the elements must be vectors of coefficients (starting with the intercept , the same
#' structure as lm()$coefficients or ds.glm()$coefficients). In this case the residuals will be directly calculated
#' on each node. Only simple models are supported without interactions (ex: Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width)
#' @param indvars a vector of column names, the predictors used for all models. A name can be present both here and in
#' outcomes, if this is the case it will be eliminated from the predictors when needed.
#' @param data the name of the data frame containing the columns.
#' @param newobj the name of the new data frame containing the residuals. The column names of this data frame
#' will be the column names in 'outcomes'

#' @param async a logical, see datashield.aggregate

#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @export
#'


dssLmResid <- function(outcomes, indvars, data, newobj = 'residuals',  async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  outcomes <- .encode.arg(outcomes)
  indvars <- .encode.arg(indvars)
  exp <- list(as.symbol('residLmDSS'), outcomes = outcomes, indvars = indvars, data = as.symbol(data))
  datashield.assign(datasources, newobj, as.call(exp), async = async)
}




