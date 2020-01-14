test_that("dssSuggestPivot  works", {
  test$locals$local1$envir$iris1 <-test$locals$local1$envir$iris
  test$locals$local1$envir$iris1$id <- 1:nrow(test$locals$local1$envir$iris1)
  test$locals$local1$envir$iris_melt <- suppressWarnings(reshape2::melt(test$locals$local1$envir$iris1,'id' ))
  x <- dssSuggestPivot('iris_melt', by.col = 'id', datasources = opals[1])
  expect_equal(x$local1, "id ~ variable")

})

test_that("dssPivot  works", {
  dssPivot('wide_iris', 'iris_melt', 'value', by.col = 'id', datasources = opals[1])
  test$locals$local1$envir$new_iris <- test$locals$local1$envir$wide_iris[,c(4,5,2,3,6,1)]
  colnames(test$locals$local1$envir$new_iris) <- colnames(test$locals$local1$envir$iris1)
 x <-  data.frame(lapply(test$locals$local1$envir$new_iris, as.character ), stringsAsFactors = FALSE)
 y <-  data.frame(lapply(test$locals$local1$envir$iris1, as.character ), stringsAsFactors = FALSE)
    expect_equal(x, y)
})
