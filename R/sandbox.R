
options("nfilter.tab" = 1)
options("nfilter.subset" = 1)
options("nfilter.glm" = 1)
options("nfilter.string" = 1000)
options("nfilter.stringShort" = 1000)
options("nfilter.kNN" = 1)
options("nfilter.levels" = 1)
options("nfilter.noise" = 0)
options("datashield.privacyLevel" = 1)

#' @title Fake opal servers in R
#' @description Create pseudo opal/datashield servers in the local session for fun and profit.
#' @param opal_name required, a character. The name of a new list (created by the function as a side effect)
#' containing the pseudo servers.
#' @param servers either the number of servers or a vector containing their names
#' @param tie_first_to_GlobalEnv a logical, should the first server session be the same as .GlobalEnv? See details.
#' @details The function creates a list object containing the local pseudo servers as elements. The names of the servers
#' are either provided in the "servers" parameter or created as 'local1', 'local2' etc. Each "server" is an environment.
#' If tie_first_to_GlobalEnv is set to TRUE, the first server in the list will be a reference to the global environment.
#' This means that all the objects in .GlobalEnv will become available to datashield... methods.
#' Lastly, the function overrides datashield.login from the package opal.
#' @seealso \code{\link{datashield.login}}
#' @return a vector containing the server names. This vector (or a subset) will be used by datashield.login as first parameter.
#' @export
dssCreateFakeServers <- function(opal_name, servers = 1, tie_first_to_GlobalEnv = FALSE){
  if(missing(opal_name)){
    stop('opal_name is required....')
  }
  first <- list()
  if(is.numeric(servers) && length(servers) ==1){
    for (i in 1:servers){
      l <- paste0('local',i)
      first[[l]] <- new.env(parent = .GlobalEnv)
      class(first[[l]]) <- c('local')
      first[[l]]$name <- l
      if(tie_first_to_GlobalEnv && i == 1){ # optionally the first envir is globalenv
        first[[l]]$envir <- .GlobalEnv
      } else {
        first[[l]]$envir <- new.env(parent = .GlobalEnv)
      }

    }
  } else {
    first <- Map( function(x){
      ret<- new.env(parent = .GlobalEnv)
      class(ret) <- c('local')
      ret$name <- x
      ret$envir <- new.env(parent = .GlobalEnv)
      ret
    }, servers)
    if(tie_first_to_GlobalEnv){
      first[[1]]$envir <- .GlobalEnv
    }

  }
  .set.new.login.function(first, opal_name)
  .set.new.datashield.methods(opal_name)
  assign(opal_name, list(locals = first), envir = .GlobalEnv)
  names(first)
}

.set.new.login.function <- function(local_conns, opal_name){

  mylogin <- function(which_connections = names(local_conns), ...){
    reals <- list()
    inputs <- list(...)
    wh <- intersect(which_connections, names(local_conns))
    local_conns <- local_conns[wh]
    final_conn_obj <-  get(opal_name, envir = .GlobalEnv)
    if (length(inputs) > 0){
      logindf <- inputs[[1]]
      if(length(final_conn_obj$remotes) > 0 ){
        logindf <- logindf[!(logindf$server %in% names(final_conn_obj$remotes)),] # do not connect twice to the same server
        inputs[[1]] <- logindf
      }
      if(nrow(inputs[[1]]) > 0 ){
        reals <- do.call('datashield.login', inputs, envir = asNamespace('opal'))
        final_conn_obj$remotes[names(reals)] <- reals
      }
    }
    if(length(wh) == 0 && is.null(reals)){
      stop('No new connections provided.')
    }

    assign(opal_name, final_conn_obj, envir = .GlobalEnv)
    out <- c(wh, names(reals))
    names(out) <- out
    attr(out, 'connection_object') <- opal_name
    out
  }
  assignInMyNamespace('datashield.login', mylogin)
  #assign('datashield.login', mylogin, envir = .GlobalEnv)

}


.set.new.datashield.methods <- function(opal_name){



  # define methods for
  # datashield.assign.character and datashield.aggregate.character and datsahield.symbols.character, then export them in the global env
  # same for datashield.<...>.local

  assn <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE){
    # the parameters after value are not used
    my.env <- opal$envir

    if(!is.call(value)){
      value <- parse(text = as.character(value))
    }

    assign(symbol, eval(value, envir = my.env), envir = my.env)

  }

  agg <- function(opal, expr, async=TRUE, wait=TRUE){
    # async and wait are there just for show
    my.env <- opal$envir

    if(!is.call(expr)){
      expr <- parse(text = as.character(expr))
    }

    eval(expr, envir = my.env)

  }

  sym <- function(opal){
    my.env <- opal$envir
    ret <- unlist(lapply(ls(envir = my.env), function(x) if(class(eval(parse(text=x), envir = my.env)) != 'function') x))
    if(is.null(ret)){
      ret <-character(0)
    }
    ret
  }

  sym.char <- function(opals_vector){
    conn_obj <- get(opal_name, envir = .GlobalEnv)
    Map(function(x){
      datashield.symbols(x)
    },
    Reduce(c,conn_obj)[opals_vector]
    )
  }

  assn.char=function(opals_vector, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {

    conn_obj <- get(opal_name, envir = .GlobalEnv)
    Map(function(x){
      datashield.assign(x, symbol, value, variables, missings, identifiers , async, wait)
    }, Reduce(c,conn_obj)[opals_vector])
    invisible()
  }

  agg.char=function(opals_vector, expr, async=TRUE, wait=TRUE) {
    conn_obj <- get(opal_name, envir = .GlobalEnv)

    Map(function(x){
      datashield.aggregate(x, expr)
    },
    Reduce(c,conn_obj)[opals_vector]
    )
  }

  logout <- function(opals_vector){
    conn_obj <- get(opal_name, envir = .GlobalEnv)

    rem <- conn_obj$remotes
    to_logout <- rem[opals_vector]
    to_logout <- to_logout[!sapply(to_logout, is.null)]
    if(length(to_logout) > 0){
      opal::datashield.logout(to_logout)
    }
    conn_obj$remotes[names(to_logout)] <- NULL
    assign(opal_name, conn_obj, envir = .GlobalEnv)
    rm(list = ls(pattern ='datashield.*.local|datashield.*.opal|datashield.*.character|print.local', envir = .GlobalEnv), envir = .GlobalEnv)
  }

  assign('datashield.assign.local', assn, envir = .GlobalEnv)
  assign('datashield.aggregate.local', agg, envir = .GlobalEnv)
  assign('datashield.assign.character', assn.char, envir = .GlobalEnv)
  assign('datashield.aggregate.character', agg.char, envir = .GlobalEnv)
  assign('datashield.assign.opal', opal:::datashield.assign.opal, envir = .GlobalEnv)
  assign('datashield.aggregate.opal', opal:::datashield.aggregate.opal, envir = .GlobalEnv)
  #assign('datashield.aggregate.list', agg.list, envir = .GlobalEnv)
  assign('datashield.symbols.local', sym, envir = .GlobalEnv)
  assign('datashield.symbols.character', sym.char, envir = .GlobalEnv)
  assign('datashield.symbols.opal', opal:::datashield.symbols.opal, envir = .GlobalEnv)
  assign('datashield.logout', logout, envir = .GlobalEnv)
  assign('print.local', function (x, ...)
  {
    cat("url: local","\n")
    cat("name:", x$name, "\n")
    cat("content: ")
    sapply(ls(envir = x$envir), function(y) cat(y,"(", class(get(y, envir = x$envir)), ') ', sep = ""))
    cat("\n")
  }, envir = .GlobalEnv)


}


#' @title Extends datashield.login from package opal
#' @description Creates the necessary software infrastructure for the usual opal and datashield functions to be able to run in
#' local pseudo-sessions as well as in remote real sessions
#' @param which_connections optional, a vector containing the names of the local "sessions" to "connect" to. Normally the output
#' of dssCreateFakeServers (or a subset of it)
#' @param ... optional, the parameters for opal::datashield.login (logins dataframe, etc) in case we want to connect to any remote, real opal servers
#' @details This function creates the final connection object and assigns it in the global environment. Moreover:
#' * If necessary establishes connections to remote, real servers
#' * Creates variants of the generics datashield.assign , datashield.aggregate and datashield.symbols that work on the local pseudo sessions.
#' * Returns a vector that can be used in various dsBaseClient (and other) functions. Attention: these functions will work only if
#' called with an explicit datasources parameter (ex: ds.var('some_vector', **datasources = myopals**) and not: ds.var('some_vector'))
#' @seealso \link{dssCreateFakeServers}
#' @return a vector containing the names of all the establised connections (real and fake)
#' @examples
#' # Mixed opal connections, local(fake, in my session) + remote(real)
#' # Read a real connection dataframe from a file
#' logindata <- read.delim('/path to your/logindata.txt')
#' # say it looks like this:
#' logindata
#'   server                     url       user      password    table
#'1  real1  https://remote.opal.node  some_user   some_pass   proj.TABLE
#' # create 2 local connections:
#' x <- dssCreateFakeServers('all_conns',c('fake1', 'fake2'))
#' #login:
#' opals <- datashield.login(x,logindata)
#' # opals contains 3 connections, 2 locals, one remote. You can examine them:
#' opals
#' all_conns
#' # create a function called partial_data in the local session as well as on the remote node
#' # on the remote node this function must be published as an aggregate method:
#'partial_data <- function (what, start = NULL, end = NULL) {
#'  data(list = list(what), envir = environment())
#'  my.df <- get(what)
#'  if (is.null(start)) {
#'    start <- 1
#'  }
#'  if (is.null(end)) {
#'    end <- nrow(my.df)
#'  }
#'  my.df <- my.df[start:end, ]
#'  assign(what, my.df, pos = parent.frame())
#'  TRUE
#'}
#' the above loads a piece of an R dataset
#' # we can use it to load 3 chunks of the 'iris' dataset, each on a different node
#' datashield.aggregate(opals['fake1'], as.symbol('partial.data("iris", 1, 40)'))
#' datashield.aggregate(opals['fake2'], as.symbol('partial.data("iris", 41, 100)'))
#' datashield.aggregate(opals['real1'], as.symbol('partial.data("iris", 101, 150)'))
#' # run some ds... commands:
#' ds.summary('iris', datasources = opals)
#' ds.mean('iris', datasources = opals) # always specify the datasources explicitly
#' # where's my local data?
#' ls(envir = all_conns$locals$fake1$envir)
#' ls(envir = all_conns$locals$fake2$envir)
#'
#' datashield.logout(opals)
#' @export

datashield.login <- function(which_connections = names(local_conns), ...){

  # Here just for the documentation. The real function is created on the fly by dssCreateFakeServers
}



