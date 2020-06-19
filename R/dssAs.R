#' @title Coerce to new type (like R as... functions)
#' @param newtype a character, what type to convert to to (ex: "character" or "data.frame")
#' @param object a character, name of the object to convert
#' @param newobject a character, optional, name of the newly created object. If missing the old object will be overwritten.
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return  TRUE if succesful
#' @examples
#' dssAs('data.frame', 'some_matrix_object', 'my_new_df',  datsources = opals)
#' @export
#'


dssAs<- function(newtype, object, newobject = NULL,  async = TRUE, wait = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  if(is.null(newobject)){
    if(grepl(object, '$', fixed = TRUE)){
      stop('I cannot convert a data frame column in place. Please specify newobject.')
    }
    newobject <- object
  }
  object <-.encode.arg(object)
  cally <- paste0('asDSS("', newtype,'","', object, '")')
  datashield.assign(datasources, newobject, as.symbol(cally), async = async, wait = wait)
}
