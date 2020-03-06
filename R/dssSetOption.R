#' @title Set  options in remote R sessions
#' @param option_list a list describing the options to be set
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return  TRUE if succesful
#' @examples
#' dssSetOption(list(stringsAsFactors = TRUE, digits = 2), datasources = my_datasources)
#' @export
#'


dssSetOption <- function(option_list,  async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }

  expr <- list(as.symbol('setOptionDSS'), .encode.arg(option_list))

  opal::datashield.aggregate(datasources,as.call(expr), async=async, wait = wait)

}
