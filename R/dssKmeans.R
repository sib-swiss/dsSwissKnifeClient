#' @title  Kmeans on datashield nodes
#' @description Runs kmeans on the remote data, returns a kmeans object representing the cluster centers in either split or combined mode
#' @param what a character, name of the dataframe (it can contain non-numerics in which case only the numeric columns will be used)
#' @param centers either a number (k - the number of clusters) or a matrix representing the initial number of initial distinct cluster centers (same as for kmeans)
#' @param iter.max same as kmeans, maximum number of iterations
#' @param nstart same as kmeans, if centers is a number, how many random sets should be chosen
#' @param type a character, 'split' or 'combine', should it find the global cluster centers or one set for each node? Default 'combine'.
#' @param algorithm same as kmeans, it defaults to "Forgy" as it's the only one that doesn't error out in the case of empty clusters
#' @details If type = 'split' the function simply executes kmeans with the provided arguments and returns one set of cluster centers for each node.
#' If type = 'combine', and centers are provided as a number it first chooses a set of random initial centers from the ranges of the combined dataset, then
#' it executes exactly one iteration of kmeans (with these initial centers) on each node. The results are then retrieved, averaged and the newly obtained centers
#' are sent to the nodes for a new iteration. The process continues until iter.max is reached. If nstart > 1 (recommended for any meaningful results), a new random set of initial centers
#' is calculated and so on until nstart is reached. Then the 'best' cluster centers are chosen as being the ones with the lowest within cluster sum of squared distances.
#' In both cases ('split' and 'combine') a factor representing the cluster membership of each point is created on the nodes. The name of the factor is derived
#' from the dataframe name: <dataframe name>_km_clust<number of clusters>.
#' If iter.max is 0 and centers is a matrix the function simply creates the cluster membership factor (as above) using the given centers.
#' @param async same as in datashield.assign
#' @param wait same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return A list containing one (in the case of 'combined') or more ('split') stripped down kmeans objects.
#' @examples
#' # open a local pseudo connection:
#' x <- dssCreateFakeServers('test', servers = 2, tie_first_to_GlobalEnv = 1)
#' opals <- datashield.login(x)
#' #load 2 chunks of iris, one on each node:
#' datashield.aggregate(opals[1], as.symbol('partial.data("iris", 1, 70)'))
#' datashield.aggregate(opals[2], as.symbol('partial.data("iris", 71, 150)'))
#' #combined kmeans:
#' my.kmeans <- ds2.kmeans('iris', centers = 3, iter.max =30, nstart = 30, type = 'combine', datasources = opals)
#' #compare it with simple kmeans on iris:
#' data("iris")
#' #kmeans allows only numeric data, so no 'Species', hence iris[,1:4]
#' local.kmeans <- kmeans(iris[,1:4], centers = 3, iter.max = 30, nstart = 30, algorithm = 'Forgy')
#' my.kmeans$global$centers
#' local.kmeans$centers
#'


dssKmeans <- function(what, centers, iter.max = 10, nstart = 1, type = 'combine', algorithm = "Forgy",
                       async = TRUE, wait = TRUE, datasources = NULL){

  if(is.null(datasources)){
    datasources <- dsBaseClient_findLoginObjects()
  }
  if(iter.max == 0 ){
    if(!is.matrix(centers)){
      stop('For simple cluster allocation (no iterations) "centers" must be a matrix.')
    }
    expr <- list(as.symbol('partialKmeans'), what, .encode.arg(as.data.frame(centers)), NULL, TRUE)
    # kms <- datashield.aggregate(datasources, as.symbol(expr), async = async, wait = wait)
    return(datashield.aggregate(datasources, as.call(expr), async = async, wait = wait))
  }
  if(type == 'split'){ # execute on each node and get out
    expr <- paste0('partialKmeans("', what, '","',.encode.arg(centers) ,'",NULL, FALSE, TRUE,', iter.max, ',', nstart, ',"', .encode.arg(algorithm) ,'")')
    km <- datashield.aggregate(datasources, as.symbol(expr), async = async, wait = wait)
    return(km)
  } else if (type == 'combine'){
    # kmeans implementation on partitioned data
    # execute exactly one iteration of kmeans on the remote nodes, return the intermediary centroids, average, rinse and repeat
    # logic in .do_kmeans
    # we have to send the same initial centroids to all nodes
    #ranges <- NULL
    k <- NULL
    driving.node <- NULL # node that sets the initial centers
    # first calculate the ranges, we'll use them later to sample random new centers in the eventuality of empty clusters
    expr <- paste0('partRange("', what, '")')
    range_list <- datashield.aggregate(datasources, as.symbol(expr), async = TRUE, wait = TRUE)
    ranges <- apply(as.data.frame(Reduce(rbind, sapply(range_list, function(x) unlist(x, recursive = FALSE), simplify = FALSE))), 2 , range)

    if(length(centers) == 1L) { # it's a number
      k <- centers
      first.km <- .init_centers(what, k, datasources, algorithm, driving.node)
      driving.node <- names(first.km)
      centers <- first.km[[1]]$centers
    } else {
      centers <- as.matrix(centers)
      k <- nrow(centers)
      if(any(duplicated(centers)))
        stop("initial centers are not distinct")
    }
    # give them names for later usage in partialKmeans
    rownames(centers) <- paste0('c', 1:k)

    global.means <- dssColMeans(what, type = 'combine', datasources = datasources)
    global.means <- global.means$global$means
    this.km <- .do_kmeans(what, centers, iter.max, global.means, ranges, async, wait, datasources)
    #bad.km <- list()
    if(nstart >= 2){

      for(i in 2:nstart){
        message(paste0('Start ', i, ' ...'))
        first.km <- .init_centers(what, k, datasources, algorithm, driving.node)
        driving.node <- names(first.km)
        centers <- first.km[[1]]$centers
        rownames(centers) <- paste0('c', 1:k)
        new.km <- .do_kmeans(what, centers, iter.max, global.means, ranges, async, wait, datasources)
        # keep the one with the lowest withinss
        if(new.km$tot.withinss < this.km$tot.withinss){
          message(paste0('Keeping start ', i))
          #bad.km[[i-1]] <- this.km
          this.km <- new.km
        } #else {
        #  bad.km[[i-1]] <- new.km
        #}
      }
    }
    #expr <- paste0('partialKmeans("', what, '","', .encode.arg(as.data.frame(this.km$centers)), '", NULL, TRUE)' )
    #datashield.aggregate(datasources, as.symbol(expr), async = async, wait = wait)
    #build expr as a list to be sent as.call

    expr <- list(as.symbol('partialKmeans'), what, .encode.arg(as.data.frame(this.km$centers)), NULL, TRUE)
    # kms <- datashield.aggregate(datasources, as.symbol(expr), async = async, wait = wait)
    kms <- datashield.aggregate(datasources, as.call(expr), async = async, wait = wait)

    #return(list(good = this.km, bad = bad.km))
    return(list(global = this.km))
  }
}


.init_centers <- function(what,centers, datasources, algo, firstnode = NULL){
  # find the node with the most lines
  if(is.null(firstnode)){
    dims <- dssDim(what, datasources = datasources)
    firstnode <- names(which.max(sapply(dims, function(x) x[1], simplify = FALSE)))
  }
  expr <- paste0('partialKmeans("', what, '","',.encode.arg(centers) ,'",NULL, FALSE, TRUE,', 1, ',', 1, ',"', .encode.arg(algo) ,'")')
  km <- datashield.aggregate(datasources[firstnode], as.symbol(expr), async = FALSE, wait = TRUE)
  return(km)

}


.do_kmeans <- function(what, centers, iter.max = 10, mns, ranges, async = TRUE, wait = TRUE, datasources = NULL ){

  #initialization
  old.centers <- centers
  old.centers[] <- 0
  new.centers <- centers
  it <- 0
  # reducer function  - calculates weighted means (..$cluster contains counts of points in each intermediary cluster)
  reducer <- function(x, y){
    z <- y

    z$cluster <-cl <- y$cluster + x$cluster

    #z$size <-cl <- y$size + x$size
    cl[cl == 0] <-1 # to avoid division by 0, should be harmless

    z$centers <- (y$centers * y$cluster  + x$centers *x$cluster)/cl
    #z$centers <- (y$centers * y$size  + x$centers *x$size)/cl
    z
  }
  # now iterate (at least once, until the result doesn't change any more or we reached iter.max)
  while(it == 0 || (any(sapply(rownames(old.centers), function(x){
    dist(rbind(old.centers[x,],new.centers[x,]))
  }) > 1e-05)  && it < iter.max)){
    it <- it + 1
    message(paste0('    Iteration ', it, ' ...'))
    #expr <- paste0('partialKmeans("', what, '","', .encode.arg(as.data.frame(new.centers)), '")')
    #build expr as a list to be sent as.call

    expr <- list(as.symbol('partialKmeans'), what, .encode.arg(as.data.frame(new.centers)))
    # kms <- datashield.aggregate(datasources, as.symbol(expr), async = async, wait = wait)
    kms <- datashield.aggregate(datasources, as.call(expr), async = async, wait = wait)
    new.km <- Reduce(reducer, kms)
    #new.km$clusters <- sapply(names(kms), function(x){
    #  kms[[x]]$cluster
    #}, simplify = FALSE)
    old.centers <- new.centers
    new.centers <- new.km$centers
    # do something about empty clusters:
    emp.ind <- names(which(apply(new.centers==0,1, all)))
    n.emp <- length(emp.ind)
    if(n.emp >0 ){
      emp.replacement <- apply(ranges,2, function(x) runif(n.emp, x[1], x[2]))
      new.centers[emp.ind,] <- emp.replacement
    }
    rownames(new.centers) <- paste0('c', rownames(new.centers))

    dif <- sapply(rownames(old.centers), function(x){
      dist(rbind(old.centers[x,],new.centers[x,]))
    })
    message(paste0('    Centers moved by ', paste(dif, collapse = ', ')))

  }

  #build expr as a list to be sent as.call

  expr <- list(as.symbol('partialKmeans'), what, .encode.arg(as.data.frame(new.centers)), .encode.arg(mns))
  sss <- datashield.aggregate(datasources, as.call(expr), async = async, wait = wait)

  new.km[names(sss[[1]])] <- Reduce(function(x,y) {
    Map(function(z){
      x[[z]] + y[[z]]
    }, names(x))
  }, sss)
  new.km$iter <- it # show the global iterations client -> nodes
  new.km$ifault <- 0 # there will always be a 'did not converge' status as we iterate only once on the nodes
  new.km$size <- new.km$cluster # I redefined it like that on the server
  return(new.km)
}
