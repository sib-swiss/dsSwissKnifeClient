test_that("dssGM works", {

  merged.mix <- dssGM('iris', NULL, 2, datasources = opals)
  merged.components = merged.mix$components

  data(iris)
  real.mix = mixtools::mvnormalmixEM(iris[,1:4], k = 2);

  expect_equal(length(merged.components), length(real.mix$mu))
  sapply(merged.components, `[[`, 'mu')
  real.mix$mu

  # These should pass, mu should be close
  #expect_equal(merged.components[[1]]$mu, real.mix$mu[[2]], 0.1)
  #expect_equal(merged.components[[2]]$mu, real.mix$mu[[1]], 0.1)

  #expect_equal(merged.components[[1]]$Sigma, real.mix$sigma[[2]], 0.01)
  #expect_equal(merged.components[[2]]$Sigma, real.mix$sigma[[1]], 0.01)

})

