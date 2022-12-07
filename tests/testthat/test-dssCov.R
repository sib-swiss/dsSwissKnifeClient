test_that("dssCov split works", {
  rem <- dssCov('iris', type = 'split', datasources = opals)
  expect_equal(rem$server1$vcov, cov(session1$iris[,1:4]))
  expect_equal(rem$server2$vcov, cov(session2$iris[,1:4]))
})

test_that("dssCov combined works", {
  rem <- dssCov('iris', type = 'combine', datasources = opals)
  expect_equal(rem$global$vcov, cov(iris[,1:4]))
})


test_that("dssCov weighted works", {
  w <- c(0.1, 0.5, 0.4, rep(0, (nrow(session1$iris)-3)))
  dssUpload('w', datasources = opals[1])
  cov_iris1 <- cov.wt(session1$iris[,1:4], wt = session1$w )
  rem <- dssCov('iris', type = 'split', wt = 'w',  datasources = opals[1])
  expect_true(all(rem$server1$cov - cov_iris1$cov == 0))
})

test_that("dssCov weighted works with weights in a data frame embedded in a list", {
  w <-list(dw = data.frame(wt = c(0.1, 0.5, 0.4, rep(0, (nrow(session1$iris)-3)))))
  dssUpload('w', datasources = opals[1])
  cov_iris1 <- cov.wt(session1$iris[,1:4], wt = session1$w$dw$wt )
  rm(w)
  rem <- dssCov('iris', type = 'split', wt = 'w$dw$wt',  datasources = opals[1])
  expect_true(all(rem$server1$cov - cov_iris1$cov == 0))
})
