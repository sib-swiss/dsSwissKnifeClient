#' @title Call selected functions from the package uwot
#' @description Only the functions 'umap'  and 'umap_transform' are implemented
#' @param func a character, the name of the function to call
#' @param X, a character, the name of the dataframe containing the input data. The non-numeric columns are
#' automatically stripped of before running the function.
#' @param model a umap model, used only for the function umap_transform. This can be: a model in the current (local) session,
#' the path to a saved model (with uwot::save_model()) on the local disk or the path to a saved model on the server storage
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
      } else { # it's already on disk, not sure if here or there yet
        fname <- model
        if(file.exists(fname)){ # it's here
          file_is_local <- TRUE
        }
      }
      if(file_is_local){
        uwot_model <- readBin(fname, 'raw', n = file.info(fname)$size)
        destfile <- 'uwot_model'
        dssUpload(destfile, maxsize = 1e+7, async, datasources) # upload 10 megs at a time
      } else {
        destfile <- model # we have a model already out there
      }
      expr$model <- .encode.arg(destfile)
    }
  }

  response <- datashield.aggregate(datasources, as.call(expr), async = async)

#  sapply(names(datasources), function(x){
#    rfname <- tempfile(pattern='uwotDSS', tmpdir = tempdir(check=TRUE))
#    opal.file_download(datasources[[x]]@opal, response[[x]], rfname)
#    uwot::load_uwot(rfname)
#    }, simplify = FALSE)
  if(!is.list(response)){ # buggy datashield.aggregate doesn't return a list if it's a single node
    node <- names(datasources) # it's only one
    tmp <- list()
    tmp[[node]] <- response
    response <- tmp
  }
  if(func == 'umap'){ # the model is tricky, it comes as a blob
    response <- sapply(names(response), function(x){
      rfname <- tempfile(pattern='uwotDSS', tmpdir = tempdir(check=TRUE))
      writeBin(response[[x]], rfname,  useBytes = TRUE)
      uwot::load_uwot(rfname)
    }, simplify = FALSE)
  }
  return(response)
}
