test_that("dssJoin works", {
  session1$iris_id1  <- session1$iris
  session1$iris_id1$id <- 1:nrow(session1$iris)
  session1$iris_id2 <- session1$iris_id1
  dssJoin(c('iris_id1', 'iris_id2'), symbol = 'joined_iris_id',
          by = dplyr::join_by('id', closest('Sepal.Length' <= 'Sepal.Length')), join.type = 'inner', datasources = opals["server1"])
  expect_equal(unname(session1$iris_id1), unname(session1$joined_iris_id[,1:6]))

})








