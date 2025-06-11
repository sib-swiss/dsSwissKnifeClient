test_that("dssRemoveOutliers  works with sigmas", {
  session1$iris_with_nas <- session1$iris
  dssRemoveOutliers('iris_with_nas', sigmas = 1 , datasources = opals[1])
  expect_equal(session1$iris_with_nas$Sepal.Length[3], as.double(NA))
})


test_that("dssRemoveOutliers  works with atomic minval/maxval", {
  session1$iris_with_nas <- session1$iris
  dssRemoveOutliers('iris_with_nas', collist = c('Sepal.Width', 'Sepal.Length'), minval = 1, maxval=4 , datasources = opals[1])
  expect_equal(max(session1$iris_with_nas$Sepal.Width, na.rm = TRUE), 4)

})

test_that("dssRemoveOutliers  works with vectorized minval/maxval", {
  session1$iris_with_nas <- session1$iris
  dssRemoveOutliers('iris_with_nas', minval = c(1,2,1,1), maxval=c(5,4,2,2) , datasources = opals[1])
  expect_equal(session1$iris_with_nas$Sepal.Length[1], as.double(NA))
})

