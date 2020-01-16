test_that("dssJoin works", {
  test$locals$local1$envir$iris_id1  <- test$locals$local1$envir$iris
  test$locals$local1$envir$iris_id1$id <- 1:nrow(test$locals$local1$envir$iris)
  test$locals$local1$envir$iris_id2 <- test$locals$local1$envir$iris_id1
  dssJoin(c('iris_id1', 'iris_id2'), symbol = 'joined_iris_id', by = 'id', join.type = 'inner', datasources = opals[1])
  expect_equal(unname(test$locals$local1$envir$iris_id1), unname(test$locals$local1$envir$joined_iris_id[,1:6]))

})








