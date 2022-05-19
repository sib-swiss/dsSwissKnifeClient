

#'
#' @title Representation of a Gaussian component.
#' @description Represents a set of normally-distributed points in R^n
#'     by their mean vector, covariance matrix and number of data points.
#'
.Gaussian <- setRefClass("Gaussian", fields = list(
    mu = "numeric",     # mean vector
    Sigma = "matrix",   # covariance matrix
    size = "numeric"    # sample size
))

#'
#' @title Representation of a Gaussian Mixture.
#' @description Just a wrapper on a list of Gaussian - to better understand what is in the list.
#'
.Mixture <- setRefClass("Mixture", fields = list(
    components = "list"   # list of Gaussian
))

#'
#' @title Call mixtools on each source, then merge the Gaussian mixtures from them.
#' @description
#'     Assume that data from a pool of individuals is split among different data sources,
#'     i.e. their initial distribution is the same and we want to approximate it.
#'     Assume that this data can be clustered into N multivariate Gaussian distributed groups.
#'     Instead of pooling the data to run the classic EM algorithm on the pool,
#'     we run the EM on each source subset, and merge the estimated components.
#'     Calling `solve.mixtools` on a remote source returns a Mixture.
#'     We merge all the Mixtures using `.merge.client`.
#' @param what: The data frame.
#' @param cols: Vector of column names for the numeric variables to consider.
#' @param K: the number of expected Gaussian components in the mix.
#'
dssGM <- function(what, cols = NULL, K, async = TRUE, wait = TRUE, datasources = NULL) {
    if(is.null(datasources)){
        datasources <- datashield.connections_find()
    }
    expr <- paste0('gmDSS(', what)
    cols.arg <- .encode.arg(cols)
    expr <- paste0(expr, ', "', cols.arg , '"')
    K.arg <- .encode.arg(K)
    expr <- paste0(expr, ', "', K.arg , '"', ')')
    mixtures = datashield.aggregate(datasources, as.symbol(expr), async, wait)
    mixture = .merge.client(mixtures)
}


#'
#' @title Merge the means from the Gaussian mixture models from multiple sources.
#' @description For each of the K components, do a weighted average of the means
#'     from all sources for that component.
#' @param mixtures: a list of Mixtures.
#' @return a Mixture.
#'
.merge.client <- function(mixtures) {
    # Transpose: N sources with K components --> K components with N means to merge.
    components = .transpose(mixtures)
    # Merge: K components with N means to merge. --> K Gaussians.
    merged.components = .merge(components)
    # Make it return a Mixture
    res = .Mixture(components = merged.components)
    return(res)
}


#'
#' @title "Transposes" N sources with K components --> K components with N means to merge.
#' @description The main problem is that the order is not always the same, i.e.
#'     component 1 in source 1 can correspond to component 3 in source 2.
#'     Start with source 1 as reference. To each component of source 1,
#'     attach the closest component of source n, based on the distance between the means.
#' @param mixtures: a list of Mixtures.
#' @return a list of K components, each of which is a list of N Gaussians.
#'
.transpose <- function(mixtures) {
    mixture1 = mixtures[[1]]
    component1 = mixtures[[1]]$components[[1]]
    N = length(mixtures)  # number of sources
    K = length(mixture1$components)  # number of components in each mixture
    D = length(component1$mu)  # number of dimensions

    components = vector("list", K)  # will contain k vectors of length n, the k groups of means to merge
    # Track the Gaussians that have already been attributed to a component, in case it was close
    taken = vector("list", N)
    for (n in 2:N) {
        taken[[n]] = numeric()
    }
    for (k in 1:K) {
        #print(paste0("----------- ", k, " ---------"))
        component_k1 = mixture1$components[[k]]
        mu = component_k1$mu
        va = component_k1$Sigma
        size = component_k1$size
        components[[k]] = vector("list", N)  # will contain n objects (lists) with mean and count
        components[[k]][[1]] = .Gaussian(mu = mu, Sigma = va, size = size)
        if (N == 1) {
            next
        }
        # Find the closest mean from other sources to group similar components.
        for (n in 2:N) {
            #print(paste0("---->>> ", n, " <<<<<---"))
            components_n = mixtures[[n]]$components  # a list of K Gaussians
            # Find the index of the component with closest mean in this source
            means_n = lapply(components_n, function(comp) comp$mu)
            m2 = matrix(unlist(means_n), nrow=2)
            distances = colSums((m2 - mu)^2)
            distances[taken[[n]]] = Inf  # discard the ones already attributed
            min.idx = which.min(distances)
            #return(list(components_n, min.idx))
            assign('deb', list(k = k, n = n, minidx = min.idx, comp = components_n, dist = distances, taken_n = taken[[n]], m2 = m2, mu=mu), envir = .GlobalEnv)
            taken[[n]] = c(taken[[n]], min.idx)  # mark as already used
            # Add the closest Gaussian of the mixture to component k
            min.mean = components_n[[min.idx]]$mu
            min.Sigma = components_n[[min.idx]]$Sigma
            min.size = components_n[[min.idx]]$size
            components[[k]][[n]] = .Gaussian(mu = min.mean, Sigma = min.Sigma, size = min.size)
        }
    }
    return(components)
}


#'
#' @title Merge K components "made of" N Gaussians from N sources,
#'     into K Gaussians by averaging the N sources (with weights).
#' @description The main problem is that the order is not always the same, i.e.
#'     component 1 in source 1 can correspond to component 3 in source 2.
#'     Start with source 1 as reference. To each component of source 1,
#'     attach the closest component of source n, based on the distance between the means.
#' @param components: a list of K components, each of which is a list of N Gaussians.
#' @return a list of K components, each of which is a list of N Gaussians.
#'
.merge <- function(components) {
    N = length(components[[1]])
    D = length(components[[1]]$mu)
    merged.components = lapply(components, function(gaussians) {
        # `gaussians` is `components[[k]]`, i.e. a list 1..N of Gaussians

        ## Sum sample sizes
        merged.size = sum(unlist(lapply(gaussians, function(g) g$size)))

        ## Merge means
        merged.mean = rep(0, D)
        mean.denom = 0
        mean.num = 0
        for (i in 1:N) {
            gauss = gaussians[[i]]
            n = gauss$size
            m = gauss$mu
            mean.denom = mean.denom + n     # a scalar
            mean.num = mean.num + n*m     # a Dx1 vector
        }
        merged.mean = mean.num / mean.denom

        ## Merge covariances
        merged.var = matrix(rep(0, D*D), D, D)
        var.denom = 0
        var.num1 = 0
        var.num2 = 0
        for (i in 1:N) {
            gauss = gaussians[[i]]
            n = gauss$size
            m = gauss$mu
            v = gauss$Sigma
            var.denom = var.denom + n     # a scalar
            var.num1 = var.num1 + n*n*v  # a DxD matrix
            var.num2 = var.num2 + n*m    # a Dx1 vector
        }
        merged.var = ((var.num1 + crossprod(t(var.num2))) / (var.denom*var.denom)) - crossprod(t(merged.mean))

        return(.Gaussian(mu = merged.mean, Sigma = merged.var, size = merged.size))
    })
    return(merged.components)
}


#'
#' @title Merge means / covariances from two sources, for one of the Gaussian components.
#' @description A weighted average of (e.g. means or covariances) `a` and `b`,
#'    given that there are respectively `na` and `nb` points at the data source.
#' @param a: [any implementing '+'] value from the first source.
#' @param b: [any implementing '+'] value from the second source.
#' @param na: [int] number of data points at the first source.
#' @param nb: [int] number of data points at the second source.
#'
.weighted.average <- function(a, b, na, nb) {
    if ((na + nb) == 0) {
        return(0)
    } else {
        return((na * a + nb * b) / (na + nb))
    }
}




