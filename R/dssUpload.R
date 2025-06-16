#' @title Upload an object
#' @description Encode and upload a local object into the remote session(s). Optionally (for very large objects) the encoded stream
#' can be split and uploaded over multiple calls
#' @param objname a character, the name of object to be uploaded
#' @param maxsize optional, an integer, the maximum size in bytes of the payload for one call. If set, the object will be sent over multibple
#' calls to the server function and rebuilt in the remote session(s)
#' @param special.object.type some objects need special treatment before and after transfer.
#' For the moment this is only implemented for uwot models (see library uwot) and the corresponding
#' value is 'uwot_model'
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#'
#' @export
#'
dssUpload <- function(objname, maxsize = NULL, special.object.type = NULL, async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  x <- get(objname, envir = parent.frame())
  if(!is.null(special.object.type) && special.object.type == 'uwot_model'){
    fname <- tempfile()
    uwot::save_uwot(x, fname) # save it
    # replace the object its raw image:
    x <- readBin(fname, 'raw', n = file.size(fname))
    file.remove(fname)
  }
  stream <- .encode.arg(x, serialize.it = TRUE)
  if(!is.null(maxsize) && maxsize < nchar(stream)){
    chunks <- .splitInEqualChunks(stream, maxsize)
    last <- length(chunks)
    # first one
    expr <- list(as.symbol('uploadDSS'), name = objname, payload = chunks[1], is.first = TRUE, is.last = FALSE, spec.obj = special.object.type)
    datashield.aggregate(datasources, as.call(expr), async = async)

    # there are at least 2 chunks, if more than 2;
    if(length(chunks) > 2){
      for(this.chunk in chunks[2:(last - 1)]){
        expr <- list(as.symbol('uploadDSS'), name = objname, payload = this.chunk, is.first = FALSE, is.last = FALSE, spec.obj = special.object.type)
        datashield.aggregate(datasources, as.call(expr), async = async)
      }
    }
    # and finally:
    expr <- list(as.symbol('uploadDSS'), name = objname, payload = chunks[last], is.first = FALSE, is.last = TRUE, spec.obj = special.object.type)
    datashield.aggregate(datasources, as.call(expr), async = async)

  } else {
    # no chunks
    expr <- list(as.symbol('uploadDSS'), name = objname, payload = stream, is.first = TRUE, is.last = TRUE, spec.obj = special.object.type)
    datashield.aggregate(datasources, as.call(expr), async = async)

  }

}
