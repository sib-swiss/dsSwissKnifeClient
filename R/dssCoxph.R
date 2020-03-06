#' @title  Remote coxph + (optional) survfit.coxph
#' @description Executes coxph \{survival\} and survfit \{survival\} on the remote nodes
#' @param ... arguments to be sent to the 2 functions.
#' Note, the data argument must be a character, the name of the input dataframe.
#' If a newdata dataframe is present in the arguments, survfit will be called on the coxph object.
#' The newdata dataframe argument must be sent as text describing the arguments to a call to data.frame() (see example).
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return  A list containing the stripped down coxph model (without the call and the residuals)
#' @examples
#' # open a local pseudo connection:
#' opals <- dssCreatePseudoServers(servers = 1, tie_first_to_GlobalEnv = 1)
#' # load the lung dataset
#' datashield.aggregate(opals[1], as.symbol('partialData("lung", NULL, NULL, "survival")'))
#' #create the new.dataframe argument (note the reference to the  'lung' data frame)
#' new.df <- 'sex = c(1, 2), age = rep(mean(lung$age, na.rm = TRUE), 2), ph.ecog = c(1, 1)'
#'
#' # execute the function:
#' cox.res <- dssCoxph(formula = survival::Surv(time, status) ~ age + sex + ph.ecog, data = 'lung', new.dataframe = new.df, async = FALSE, datasources = opals[1])
#' summary(cox.res$local1$model)
#' # plot the fit:
#' plot(cox.res$local1$fit, conf.int = TRUE, col = c('blue', 'red'))
#'
#' @export
#'

dssCoxph <- function(..., async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  arglist <- list(...) # pass the args list almost as is to coxph on the local nodes
  if(typeof(arglist$formula) ==  'language'){
    arglist$formula <- Reduce(paste, deparse(arglist$formula)) # Reduce for formulas longer than 60 chars - deparse splits them into a vector
  }
  arglist <- .encode.arg(arglist)
  cally <- paste0('coxphDS("', arglist, '")')
  opal::datashield.aggregate(datasources, as.symbol(cally), async = async, wait = wait)
}
