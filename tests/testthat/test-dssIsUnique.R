test_that("dssIsUnique works", {
  rem <- dssIsUnique('iris$Petal.Length', datasources = opals)

  expect_equal(rem, list(local1=FALSE, local2=FALSE))
  opals$local1$envir$u <- c(1,2,3)
  rem <- dssIsUnique('u', datasources = opals["local1"])
  expect_true(rem$local1)
})
