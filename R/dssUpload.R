#' @title Upload an object
#' @description Encode and upload a local object into the remote session(s). Optionally (for very large objects) the encoded stream
#' can be split and uploaded over multiple calls
#' @param objname a character, the name of object to be uploaded
#' @param maxsize optional, an integer, the maximum size in bytes of the payload for one call. If set, the object will be sent over multibple
#' calls to the server function and rebuilt in the remote session(s)
#' @param async a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#'
#' @export
#'
dssUpload <- function(objname, maxsize = NULL, async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  x <- get(objname, envir = parent.frame())
  stream <- .encode.arg(x, serialize.it = TRUE)
  if(!is.null(maxsize) && maxsize < nchar(stream)){
    chunks <- .splitInEqualChunks(stream, maxsize)
    last <- length(chunks)
    # first one
    expr <- list(as.symbol('uploadDSS'), name = objname, payload = chunks[1], is.first = TRUE, is.last = FALSE)
    datashield.aggregate(datasources, as.call(expr), async = async)

    # there are at least 2 chunks, if more than 2;
    if(length(chunks) > 2){
      for(this.chunk in chunks[2:(last - 1)]){
        expr <- list(as.symbol('uploadDSS'), name = objname, payload = this.chunk, is.first = FALSE, is.last = FALSE)
        datashield.aggregate(datasources, as.call(expr), async = async)
      }
    }
    # and finally:
    expr <- list(as.symbol('uploadDSS'), name = objname, payload = chunks[last], is.first = FALSE, is.last = TRUE)
    datashield.aggregate(datasources, as.call(expr), async = async)

  } else {
    # no chunks
    expr <- list(as.symbol('uploadDSS'), name = objname, payload = stream, is.first = TRUE, is.last = TRUE)
    datashield.aggregate(datasources, as.call(expr), async = async)

  }

}
