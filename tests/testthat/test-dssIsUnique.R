test_that("dssIsUnique works", {
  rem <- dssIsUnique('iris$Petal.Length', datasources = opals)

  expect_equal(rem, list(server1=FALSE, server2=FALSE))
  session1$u <- c(1,2,3)
  rem <- dssIsUnique('u', datasources = opals["server1"])
  expect_true(rem$server1)
})
