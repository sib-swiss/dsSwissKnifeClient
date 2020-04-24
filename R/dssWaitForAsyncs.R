
#' @title Wait for previous datashield commands
#' @description It polls the current opal commands launched with async = TRUE, wait = FALSE
#' until all of them complete. Allows the launch of nonblocking datashield commands, useful in
#' web applications. The "sleep" argument must not be set too low (0.3 seconds is probably a good minimum)
#' otherwise it might look like a DOS attack and be blocked by the remote server.
#' @param sleep polling interval in seconds, default 0.5
#' @param datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return a list containing the results of the commands (if they were supposed to return anything) or raw(0)
#' @export
#'
dssWaitForAsyncs <- function(datasources, sleep = 0.5){
  work <- names(datasources)
  r <- list()
  while(length(work) >0 ){
    r <-c(r,sapply(work, function(x){
     if('local' %in% class(datasources[[x]])){
       work <<- setdiff(work, x)
       return(NULL)
     }
      if(nrow(y <- as.data.frame(datashield.commands(datasources[[x]]))) > 0){
        apply(y,1, function(z){
          # print(z)
          try( ret <- as.list(datashield.command_result(datasources[[x]], z['id'], wait = FALSE)), silent = FALSE)
          if(length(ret) == 0){
            ret <- NULL
          }
          return(ret)
        })
      } else {
        work <<- setdiff(work, x)
        return(NULL)
      }
    }, simplify = FALSE))
    Sys.sleep(sleep)
  }
  r[!sapply(r, is.null)]
}

