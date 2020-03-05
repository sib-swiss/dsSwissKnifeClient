
#' @title Encode function  arguments
#' @description Serialize to JSON, then encode base64,
#'  then replace '+', '/' and '=' in the result in order to play nicely with the opal sentry.
#'  Used to encode non-scalar function arguments prior to sending to the opal server.
#'  There's a corresponding function in the server package calle .decode_args
#' @param some.object the object to be encoded
#' @return encoded text with offending characters replaced by strings
#'
#'
.encode.arg <- function(some.object){
  encoded <- RCurl::base64Encode(jsonlite::toJSON(some.object, null = 'null'));
  # go fishing for '+', '/' and '=', opal rejects them :
  my.dictionary <- c('\\/' = '-slash-', '\\+' = '-plus-', '\\=' = '-equals-')
  sapply(names(my.dictionary), function(x){
    encoded[1] <<- gsub(x, my.dictionary[x], encoded[1])
  })
  return(paste0(encoded[1],'base64'))

}


.bw.args <- function(x, datasources=NULL){
  #global bandwidth calculation for ds2.smooth2d
  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  r <- unlist(dssRange(x, type= 'combine', datasources = datasources))
  dif <- r[2] - r[1]
  r <- c(r[1] + dif/3, r[2] - dif/3)
  varlist <- dssVar(x,  type='combine', datasources)[[1]]
  v <- varlist$var
  l <- varlist$len
  list(quarts = r, var = v, len = l)
}


dsBaseClient_findLoginObjects <-function () {
  # copied as is from dsBaseClient

  findLogin <- dsBaseClient_getOpals()
  if (findLogin$flag == 1) {
    datasources <- findLogin$opals
    return(datasources)
  }
  else {
    if (findLogin$flag == 0) {
      stop(" Are you logged in to any server? Please provide a valid opal login object! ",
           call. = FALSE)
    }
    else {
      message(paste0("More than one list of opal login object were found: '",
                     paste(findLogin$opals, collapse = "', '"), "'!"))
      userInput <- readline("Please enter the name of the login object you want to use: ")
      datasources <- eval(parse(text = userInput))
# iulian, no more class check:
      #if (class(datasources[[1]]) == "opal") {
      #  return(datasources)
      #}
    #  else {
    #    stop("End of process: you failed to enter a valid login object",
    #         call. = FALSE)
    #  }
    }
  }
}





dsBaseClient_getOpals <- function () {
  # yanked from dsBaseClient
  objs <- ls(name = .GlobalEnv)
  if (length(objs) > 0) {
    opalist <- vector("list")
    cnt <- 0
    flag <- 0
    for (i in 1:length(objs)) {
      cl1 <- class(eval(parse(text = objs[i])))
      if('pseudo' %in% cl1){ # iulian, added pseudo
        cnt <- cnt + 1
        opalist[[cnt]] <- objs[i]
        flag <- 1

      } else if ("list" %in% cl1) {
        list2check <- eval(parse(text = objs[i]))
        if (length(list2check) > 0) {
          cl2 <- class(list2check[[1]])
          for (s in 1:length(cl2)) {
            if (cl2[s] == "opal" ) {
              cnt <- cnt + 1
              opalist[[cnt]] <- objs[i]
              flag <- 1
              next
            }
          }
        }
      }
    }
    if (flag == 1) {
      if (length(opalist) > 1) {
        flag <- 2
        return(list(flag = flag, opals = unlist(opalist)))
      }
      else {
        pp <- opalist[[1]]
        opals <- eval(parse(text = pp))
        return(list(flag = flag, opals = opals))
      }
    }
    else {
      return(list(flag = flag, opals = NULL))
    }
  }
  else {
    return(list(flag = flag, opals = NULL))
  }
}






#' @title  Swap list keys
#' @description Swap the first level of names of a list with the second level (second level can be more named lists or a simple vector).
#' Values are merged if necessary (see example)
#' @param l a list
#' @return a list with swapped keys and values
#' @examples
#' x <- list(a = list(b = c(1,2)), y = list(b = c(2,3)))
#' dssSwapKeys(x)
#' @export
#'
dssSwapKeys <- function(l){

  temp <- unlist(unlist(lapply(names(l), function(x){
    if(!is.list(l[[x]])){ # no further levels
      out <- list()
      out[l[[x]]] <- x
      return(list(out))
    } else {
      lapply(names(l[[x]]), function(y){
        out <- list()
        out[[y]] <- list()
        out[[y]][[x]] <- l[[x]][[y]]
        out
      })
    }
  }), recursive = FALSE, use.names = FALSE), recursive = FALSE)

  unlist(lapply(unique(names(temp)), function(x){
    out <- list()
    out[[x]] <- Reduce(c, temp[names(temp)==x])
    out
  }), recursive = FALSE)

}





#' @title Quote words
#' @description Like perl's qw
#' @param ... names
#' @return the input names enclosed in quotes

.qw <- function(...){
  #like perl's qw
  as.character(substitute(list(...)))[-1]
}

#' @title Rescale a vector given a new range
#' @param x  a numeric vector
#' @param new.range a vector containing the new min and max
#' @param old.range (optional) the old range
#' @return the rescaled vector

.rescale <- function(x, new.range, old.range = NULL){
  new.min <- new.range[1L]
  new.max <- new.range[2L]
  if(is.null(old.range)){
    old.range <-c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  }
  (x - old.range[1])/(old.range[2] - old.range[1]) * (new.max - new.min) + new.min
}

#' @title Add opacity (alpha) to a vector of colours
#' @param old.colours colour vector
#' @param alpha opacity ratio (1 - perfectly opaque, 0- perfectly transparent)
#' @return the new colour vector with alpha

.add.alpha <- function(old.colours, alpha){
  if(missing(alpha)) stop("provide a value for alpha between 0 and 1")
  old.rgb <- col2rgb(old.colours, alpha=TRUE)
  old.rgb[4,] <- round(old.rgb[4,]*alpha)
  new.colours <- rgb(old.rgb[1,], old.rgb[2,], old.rgb[3,], old.rgb[4,], maxColorValue = 255)
  return(new.colours)
}
