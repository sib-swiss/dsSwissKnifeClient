#' @title Call selected functions from the package uwot
#' @description Only the functions 'umap'  and 'umap_transform' are implemented
#' @param func a character, the name of the function to call
#' @param X, a character, the name of the dataframe containing the input data. The non-numeric columns are
#' automatically stripped of before running the function.
#' @param model a umap model, used only for the function umap_transform
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @param ... further arguments to be passed to the function (see the documentation of the uwot package).
#' The parameters ret_nn and ret_extra are not taken into consideration as they are potentially disclosive.
#' @return a umap model (see the documentation for the respective functions)
#' @export
#'
dssUwot <- function(func, X, model = NULL, async = TRUE, datasources = NULL, ...){
  if(!(func %in% c('umap', 'umap_transform'))){
    stop(paste0(func, ' not implemented.'))
  }
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  expr <- list(as.symbol('uwotDSS'))
  expr$func <- func
  expr$X <- as.symbol(X)
  furtherargs <- list(...)
  if(length(furtherargs) > 0 ){
    arglist <- .encode.arg(furtherargs) # pass the args list almost as is to the original function on the local nodes
    expr$arglist <- arglist
  }
  if(!is.null(model)){
    if(func != 'umap_transform'){
      warning('A model is provided but will not be used.')
    } else {
      file_is_local <- FALSE
      if(is.list(model)){ # we have to save it on disk and upload it
        fname <- tempfile(pattern='dssUwot', tmpdir = tempdir(check=TRUE))
        uwot::save_uwot(model, fname)
        file_is_local <- TRUE
      } else { # it's already on disk, not sur if here or there yet
        fname <- model
        if(file.exists(fname)){ # it's here
          file_is_local <- TRUE
        }
      }
      if(file_is_local){
        destfile <- '/tmp/uwot_model'
        opal.file_upload(datasources, fname , destfile)
      } else {
        destfile <- model # we have a model already out there
      }
      expr$model <- destfile
    }
  }

  response <- datashield.aggregate(datasources, as.call(expr), async = async) # get the location of the saved model
  sapply(names(datasources), function(x){
    rfname <- tempfile(pattern='uwotDSS', tmpdir = tempdir(check=TRUE))
    opal.file_download(datasources[[x]]@opal, response[[x]], rfname)
    uwot::load_uwot(rfname)
    }, simplify = FALSE)
}
