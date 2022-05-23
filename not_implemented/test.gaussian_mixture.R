library(opal)
library(datashield)
library(datashieldclient)
library(dsCDISC)



test_that("the result from `dssGM` is close to what mixtools gives on the whole data,
        when iris is partitioned randomly (i.e. makes sense)", {
    opals <- datashield.login(logins = c(local= 4))

    cols = c("Sepal.Length", "Sepal.Width")
    cols = NULL
    K = 2
     K = 3
    set.seed(1234)
    rand.idx = runif(nrow(iris))
    idx1 = rand.idx <= 0.25
    idx2 = rand.idx > 0.25 & rand.idx <= 0.50
    idx3 = rand.idx > 0.50 & rand.idx <= 0.75
    idx4 = rand.idx > 0.75 & rand.idx <= 1.00

    datashield.assign(opals, '_', quote(set.seed(1234)))
    datashield.assign(opals, 'rand.idx', quote(runif(nrow(iris))))
    datashield.assign(opals, 'idx1', quote(rand.idx <= 0.25))
    datashield.assign(opals, 'idx2', quote(rand.idx > 0.25 & rand.idx <= 0.50))
    datashield.assign(opals, 'idx3', quote(rand.idx > 0.50 & rand.idx <= 0.75))
    datashield.assign(opals, 'idx4', quote(rand.idx > 0.75 & rand.idx <= 1.00))

    # Assign a fraction to the iris data to the "subiris" variable name on each node
    datashield.aggregate(opals, as.symbol('partial.data("iris", 1, 150)'))
    ds2.subset("subiris", "iris", row.filter = "idx1", datasources = opals[1])
    ds2.subset("subiris", "iris", row.filter = "idx2", datasources = opals[2])
    ds2.subset("subiris", "iris", row.filter = "idx3", datasources = opals[3])
    ds2.subset("subiris", "iris", row.filter = "idx4", datasources = opals[4])

    merged.mix = dssGM('iris', cols, K, async = FALSE, wait = TRUE, datasources = opals);
    merged.components = merged.mix$components

    data(iris)
    real.mix = mixtools::mvnormalmixEM(iris[,1:4], k = K);

    expect_equal(length(merged.components), length(real.mix$mu))

    # These should pass, mu should be close
    #expect_equal(merged.components[[1]]$mu, real.mix$mu[[2]], 0.1)
    #expect_equal(merged.components[[2]]$mu, real.mix$mu[[1]], 0.1)

    #expect_equal(merged.components[[1]]$Sigma, real.mix$sigma[[2]], 0.01)
    #expect_equal(merged.components[[2]]$Sigma, real.mix$sigma[[1]], 0.01)

    expect_equal(sum(sapply(merged.components, FUN=function(comp) {comp$size})), 150)
})



test_that("the result from `dssGM` is close to what mixtools gives on the whole data,
          splitting iris into 4 badly conditioned parts", {

    skip("While writing other tests")

    opals <- datashield.login(logins = c(local= 4))

    # Assign a fraction to the iris data to the "iris" variable name on each node
    datashield.aggregate(opals[1], as.symbol('partial.data("iris", 1,50)'))
    datashield.aggregate(opals[4], as.symbol('partial.data("iris", 51,70)'))
    datashield.aggregate(opals[3], as.symbol('partial.data("iris", 71,117)'))
    datashield.aggregate(opals[2], as.symbol('partial.data("iris", 118,150)'))

    # Run `ds2..Gaussian_mix` - in sync mode for debugging
    set.seed(33)
    cols = c("Sepal.Length", "Sepal.Width")
    K = 2
    merged.mix = dssGM('iris', cols, K, async = FALSE, wait = TRUE, datasources = opals)
    merged.components = merged.mix$components

    # Bring back the original "iris" data on this local, client node
    #str(iris)                     # the 'local' one
    #str(opals$local$envir$iris)   # the 'remote' one, simulated locally
    data(iris)
    #str(iris)                     # the original one

    real.mix = mixtools::mvnormalmixEM(iris[,cols], k = 2);

    expect_equal(length(merged.components), length(real.mix$mu))
    expect_equal(merged.components[[1]]$mu, real.mix$mu[[2]], 0.1)
    expect_equal(merged.components[[2]]$mu, real.mix$mu[[1]], 0.1)
    # We do not expect these to be even close because this example is very badly conditioned
    #expect_equal(merged.components[[1]]$Sigma, real.mix$sigma[[2]], 0.01)
    #expect_equal(merged.components[[2]]$Sigma, real.mix$sigma[[1]], 0.01)
    expect_equal(sum(sapply(merged.components, FUN=function(comp) {comp$size})), 150)
})


test_that("the same example works when iris is split locally (`.merge.client`)", {

    skip("While writing other tests")

    set.seed(333)
    cols = c("Sepal.Length", "Sepal.Width")
    K = 2
    s1 = iris[1:50, ]
    s2 = iris[51:70, ]
    s3 = iris[71:117, ]
    s4 = iris[118:150, ]
    m1 = gaussian_mix.DS2(s1, cols, K)
    m2 = gaussian_mix.DS2(s2, cols, K)
    m3 = gaussian_mix.DS2(s3, cols, K)
    m4 = gaussian_mix.DS2(s4, cols, K)

    res = .merge.client(list(m1, m2, m3, m4))$components

    real.mix = mixtools::mvnormalmixEM(iris[,cols], k = K);

    expect_equal(length(res), length(real.mix$mu))
    expect_equal(res[[1]]$mu, real.mix$mu[[2]], 0.1)
    expect_equal(res[[2]]$mu, real.mix$mu[[1]], 0.1)
    # We do not expect these to be even close because this example is very badly conditioned
    #expect_equal(res[[1]]$Sigma, real.mix$sigma[[2]], 0.01)
    #expect_equal(res[[2]]$Sigma, real.mix$sigma[[1]], 0.01)
    expect_equal(sum(sapply(res, FUN=function(comp) {comp$size})), 150)
})


# Tests on lower-level functions


test_that("it works with a single component from a single source.", {
    src = .Mixture(components = list(
        .Gaussian(mu = c(0, 0), Sigma = matrix(c(1,2,2,2),2,2), size = 50)
    ))
    res = .merge.client(list(src))$components
    expect_equal(sum(res[[1]]$mu - c(0, 0)), 0)
    expect_equal(res[[1]]$size, 50)
})

test_that("it works with two components from a single source.", {
    src = .Mixture(components = list(
        .Gaussian(mu = c(0,  0),  Sigma = matrix(c(10,3,3,2),2,2), size = 50),
        .Gaussian(mu = c(20, 20), Sigma = matrix(c(1,2,2,2), 2,2), size = 30)
    ))
    res = .merge.client(list(src))$components
    expect_equal(res[[1]]$mu, c(0, 0))
    expect_equal(res[[2]]$mu, c(20, 20))
    expect_equal(res[[1]]$size, 50)
    expect_equal(res[[2]]$size, 30)
})

test_that("it works with a single component split in 2 sources.", {
    Sigma1 = matrix(c(10,3,3,2),2,2)
    Sigma2 = matrix(c(1,2,2,2),2,2)
    src1 = .Mixture(components = list(
        .Gaussian(mu = c(0, 0), Sigma = Sigma1, size = 40)
    ))
    src2 = .Mixture(components = list(
        .Gaussian(mu = c(20, 20), Sigma = Sigma2, size = 10)
    ))
    res = .merge.client(list(src1, src2))$components

    # The mean is (40*0 + 10*20)/(40+10) = 4
    # The covariance, calculated manually, is
    covariance = (1/((40 + 10)^2)) * (40*40*Sigma1 + 10*10*Sigma2 + (40*c(0,0) + 10*c(20,20)) %*% t(40*c(0,0) + 10*c(20,20))) - c(4, 4) %*% t(c(4,4))

    expect_equal(sum(res[[1]]$mu - c(4, 4)), 0)
    expect_equal(res[[1]]$size, 50)
    expect_equal(res[[1]]$Sigma, covariance)
})

test_that("it works with a single component split in 2 sources,
          both sending the exact same distribution (despite the different sample sizes).", {
    Sigma = matrix(c(10,3,3,2),2,2)
    src1 = .Mixture(components = list(
      .Gaussian(mu = c(0, 0), Sigma = Sigma, size = 40)
    ))
    src2 = .Mixture(components = list(
      .Gaussian(mu = c(0, 0), Sigma = Sigma, size = 10)
    ))
    # Note: a previous dumb attempt was to change the mean and leave the same covariance.
    # But then the merged covariance will not be the same, because the distribution
    # as a whole is different.
    res = .merge.client(list(src1, src2))$components
    expect_equal(res[[1]]$mu, c(0, 0))
    expect_equal(res[[1]]$size, 50)
    # Note: actually Sigma is always different if the sample sizes are different, by this factor:
    factor = (40*40 + 10*10) / ((40 + 10)*(40 + 10))
    expect_equal(res[[1]]$Sigma, factor * Sigma)
})

test_that("it works with 3 sources with 2 components each.", {
    Sigma1 = matrix(c(10,3,3,2),2,2)
    Sigma2 = matrix(c(1,2,2,2),2,2)

    src1 = .Mixture(components = list(
        .Gaussian(mu = c(0, 0),  Sigma = Sigma1, size = 10),
        .Gaussian(mu = c(20,20), Sigma = Sigma1, size = 10)
    ))
    src2 = .Mixture(components = list(
        .Gaussian(mu = c(3, 1),  Sigma = Sigma1, size = 10),
        .Gaussian(mu = c(19,12), Sigma = Sigma1, size = 10)
    ))
    src3 = .Mixture(components = list(
        .Gaussian(mu = c(20,40), Sigma = Sigma1, size = 20),
        .Gaussian(mu = c(1, -1), Sigma = Sigma1, size = 20)
    ))

    res = .merge.client(list(src1, src2, src3))$components
    expect_equal(res[[1]]$mu, c(1.25, -0.25))
    expect_equal(res[[2]]$mu, c(19.75, 28.00))
    expect_equal(res[[1]]$size, 40)
    expect_equal(res[[2]]$size, 40)
})


######################################################################################


#'
#' @title Test with iris data, manually, with figures.
#' @description The data is split randomly in 2 groups.
#'     Plots the ellipses of the estimated .Mixture in each source, and
#'     the merged .Mixture in the 3rd plot.
#'     We expect 3 components ("Species") to be roughly equally distributed in both groups.
#'     Actually we see that setting K = 2 is better.
#' @param K: number of components (>= 2).
#' @return Nothing - just plot.
#'
test.iris.2sources <- function(K = 2) {
    data = iris
    cols = c("Sepal.Length", "Sepal.Width")
    set.seed(1234)
    rand.idx = runif(nrow(data))
    src1 = data[rand.idx <= 0.5, cols]
    src2 = data[rand.idx > 0.5, cols]

    # Use mixtools to solve the problem on both sources separately
    model1 = mvnormalmixEM(src1, k=K, epsilon = 1e-04);
    model2 = mvnormalmixEM(src2, k=K, epsilon = 1e-04);
    mix1 = model.to..Mixture(model1)
    mix2 = model.to..Mixture(model2)

    # Merge with our method
    mix = .merge.client(list(mix1, mix2))

    par(mfrow=c(3,1))
    plot(iris[,cols], type = "n")
    colors = c("black","green","blue","red","orange","grey")
    for (k in 1:K) {
        mu = mix1$components[[k]]$mu
        Sigma = mix1$components[[k]]$Sigma
        ellipse(mu, Sigma, npoints=300, col=colors[k])
    }
    title("Source 1")
    plot(iris[,cols], type = "n")
    for (k in 1:K) {
        mu = mix2$components[[k]]$mu
        Sigma = mix2$components[[k]]$Sigma
        ellipse(mu, Sigma, npoints=300, col=colors[k])
    }
    title("Source 2")
    plot(iris[,cols], type = "n")
    for (k in 1:K) {
        mu = mix$components[[k]]$mu
        Sigma = mix$components[[k]]$Sigma
        ellipse(mu, Sigma, npoints=300, col=colors[k])
    }
    title("Merged")
}


