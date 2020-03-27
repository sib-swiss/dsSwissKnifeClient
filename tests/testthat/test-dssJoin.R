test_that("dssJoin works", {
  opals$local1$envir$iris_id1  <- opals$local1$envir$iris
  opals$local1$envir$iris_id1$id <- 1:nrow(opals$local1$envir$iris)
  opals$local1$envir$iris_id2 <- opals$local1$envir$iris_id1
  dssJoin(c('iris_id1', 'iris_id2'), symbol = 'joined_iris_id', by = 'id', join.type = 'inner', datasources = opals["local1"])
  expect_equal(unname(opals$local1$envir$iris_id1), unname(opals$local1$envir$joined_iris_id[,1:6]))

})








