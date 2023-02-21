test_that("dssGM works", {
  set.seed(1234)
  cols = c("Sepal.Length", "Sepal.Width")

  mix <- dssGM('iris', cols, 3, datasources = opals['server1'])


  data(iris)
  real.mix1 = mixtools::mvnormalmixEM(session1$iris[,cols], k = 3)
  #real.mix2 = mixtools::mvnormalmixEM(session2$iris[,cols], k = 3)
  expect_true(all(mix$server1$components[[1]]$mu - real.mix1$mu[[1]] < c(1e-2, 1e-2)))

})

