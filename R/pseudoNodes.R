#' @title Fake opal servers in R
#' @description Create pseudo opal/datashield servers in the local session for fun and profit.
#' @param servers either the number of servers or a vector containing their names
#' @param tie_first_to_GlobalEnv a logical, should the first server session be the same as .GlobalEnv? See details.
#' @details The function creates and returns a list object containing the local pseudo servers as elements. The names of the servers
#' are either provided in the "servers" parameter or created as 'local1', 'local2' etc. Each "server" is an environment.
#' If tie_first_to_GlobalEnv is set to TRUE, the first server in the list will be a reference to the global environment.
#' This means that all the objects in .GlobalEnv will become available to datashield... methods.
#' @seealso \code{\link{datashield.login}}
#' @return a list containing the local "server" environments. This list can be used straight away as the "datasources" argument
#' for various dsSwissKnife or datashield or opal functions. One caveat: the "datasources" becomes mandatory, the said functions
#' will not be able to find it automatically in the enviroment.
#' @export
dssCreatePseudoServers <- function(servers = 1, tie_res_to_GlobalEnv = FALSE){

  res <- list()
  if(is.numeric(servers) && length(servers) ==1){
    for (i in 1:servers){
      l <- paste0('local',i)
      res[[l]] <- new.env(parent = .GlobalEnv)
      class(res[[l]]) <- c('local')
      res[[l]]$name <- l
      if(tie_res_to_GlobalEnv && i == 1){ # optionally the res envir is globalenv
        res[[l]]$envir <- .GlobalEnv
      } else {
        res[[l]]$envir <- new.env(parent = .GlobalEnv)
      }

    }
  } else {
    res <- Map( function(x){
      ret<- new.env(parent = .GlobalEnv)
      class(ret) <- c('local')
      ret$name <- x
      ret$envir <- new.env(parent = .GlobalEnv)
      ret
    }, servers)
    if(tie_res_to_GlobalEnv){
      res[[1]]$envir <- .GlobalEnv
    }

  }
  class(res) <- c('pseudo', 'list')
  res
}



#' @title Extends datashield.login from package opal
#' @description Allows mixing and matching of local pseudo-connections and normal opal remote connections in the same object
#' @param pseudo.conn optional, a list containing  the local "servers" to "connect" to. Normally the output
#' of dssCreatePseudoServers (or a subset of it)
#' @param ... optional, the parameters for opal::datashield.login (logins dataframe, etc). See the documentation for that function for details.
#' @seealso \link{dssCreatePseudoServers}
#' @return an opal object containing 0 or more local pseudo-connections and 0 or more real opal connections
#' @examples
#' # Mixed opal connections, local(fake, in my session) + remote(real)
#' # Read a real connection dataframe from a file
#' logindata <- read.delim('/path to your/logindata.txt')
#' # say it looks like this:
#' logindata
#'   server                     url       user      password    table
#'1  real1  https://remote.opal.node  some_user   some_pass   proj.TABLE
#' # create 2 local connections:
#' locals <- dssCreatePseudoServers(c('fake1', 'fake2'))
#' #login the the remote node:
#' opals <- datashield.login(locals, logindata)
#' # opals contains 3 connections, 2 locals, one remote. You can examine them:
#' opals
#' #' # we can use it to load 3 chunks of the 'iris' dataset, each on a different node
#' datashield.aggregate(opals['fake1'], as.symbol('partialData("iris", 1, 40)'))
#' datashield.aggregate(opals['fake2'], as.symbol('partialData("iris", 41, 100)'))
#' datashield.aggregate(opals['real1'], as.symbol('partialData("iris", 101, 150)'))
#' # load dsBaseClient and run some ds... commands:
#' library(dsBaseClient)
#' ds.summary('iris', datasources = opals)
#' ds.levels('iris$Species', datasources = opals)
#' # and some dss... ones:
#' dssShowFactors('iris')
#' dssCov('iris')
#' # where's my local data?
#' ls(envir = opals$fake1$envir)
#' ls(envir = opals$fake2$envir)
#' # or
#' opals[c('fake1', 'fake2')]
#'
#' datashield.logout(opals)
#' @export

datashield.login <- function(x, ...){
  UseMethod('datashield.login')
}

datashield.login.pseudo <- function(pseudo.conn = NULL, ...){
  fake <- pseudo.conn
  real <- NULL
  if (length(list(...)) >0 ){
    real <- datashield.login(...)
  }
  c(real, fake)
}

datashield.login.data.frame <- function(x,...){
  opal::datashield.login(x,...)
}

datashield.assign <- function (opal, symbol, value, variables = NULL, missings = FALSE,
                               identifiers = NULL, async = TRUE, wait = TRUE, tibble = FALSE) {
  UseMethod('datashield.assign')
}

datashield.aggregate <- function(opal, expr, async=TRUE, wait=TRUE){
  UseMethod('datashield.aggregate')
}

datashield.symbols <- function(opal){
  UseMethod('datashield.symbols')
}

datashield.assign.list  <- function(opals, symbol, value, variables=NULL, missings=FALSE,
                                    identifiers=NULL, async=TRUE, wait=TRUE, tibble = FALSE) {

  res <- lapply(opals, FUN = datashield.assign, symbol,
                value, variables = variables, missings = missings, identifiers = identifiers,
                async = async, wait = FALSE, tibble = tibble)
  real_res <- NULL
  if (async && wait) {
    real_opals <- lapply(opals, function(x){
      if(class(x) == 'opal'){
        return(x)
      }
    })
    real_opals <- real_opals[!sapply(real_opals, is.null)]
    if(length(real_opals > 0 )){
      real_res <- res[names(real_opals)]
      opal::datashield.command(real_opals, real_res, wait = TRUE)
      opal::datashield.command_rm(real_opals, real_res)
    }

  }
  invisible()
}

datashield.aggregate.list <- function(opals, expr, async=TRUE, wait=TRUE) {

  res <- lapply(opals, FUN = datashield.aggregate, expr,
                async = async, wait = FALSE)
  real_res <- NULL
  if (async && wait) {
    real_opals <- lapply(opals, function(x){
      if(class(x) == 'opal'){
        return(x)
      }
    })
    real_opals <- real_opals[!sapply(real_opals, is.null)]

    if(length(real_opals) > 0 ){
      real_res <- res[names(real_opals)]
      real_res <- datashield.command_result(real_opals, real_res, wait = TRUE)
    }
  }
  if(!is.null(real_res)){
    res[names(real_res)] <- real_res
  }
  return(res)
}


datashield.symbols.list <- function(opals){
  lapply(opals, FUN = datashield.symbols)
}


datashield.assign.local <- function(opal, symbol, value, variables=NULL, missings=FALSE,
                                    identifiers=NULL, async=TRUE, wait=TRUE, tibble = FALSE){
  # the parameters after value are not used
  my.env <- opal$envir

  if(!is.call(value)){
    value <- parse(text = as.character(value))
  }

  assign(symbol, eval(value, envir = my.env), envir = my.env)
}

datashield.aggregate.local <- function(opal, expr, async=TRUE, wait=TRUE){
  # async and wait are there just for show
  my.env <- opal$envir

  if(!is.call(expr)){
    expr <- parse(text = as.character(expr))
  }

  eval(expr, envir = my.env)

}

datashield.symbols.local <- function(opal){
  my.env <- opal$envir
  ret <- unlist(lapply(ls(envir = my.env), function(x) if(class(eval(parse(text=x), envir = my.env)) != 'function') x))
  if(is.null(ret)){
    ret <-character(0)
  }
  ret
}


datashield.assign.opal <- function(...){
  opal:::datashield.assign.opal(...)
}

datashield.aggregate.opal <- function(...){
  opal:::datashield.aggregate.opal(...)
}

datashield.symbols.opal <- function(...){
  opal:::datashield.symbols.opal(...)
}

print.local <- function (x){
  cat("url: local","\n")
  cat("name:", x$name, "\n")
  cat("content: ")
  sapply(ls(envir = x$envir), function(y) cat(y,"(", class(get(y, envir = x$envir)), ') ', sep = ""))
  cat("\n")
}



fuseOpals <- function(x,y){
  out <- c(x,y)
  myenv <- parent.frame()
  sapply(c(deparse(substitute(x)), deparse(substitute(y))), function(a){
    if(exists(a, envir = myenv)){
      rm(list = a, envir = myenv)
    }
  })
  out[order(sapply(out, class), decreasing = TRUE)] # I want class opal before class local in order for findloginobjects to work
}

