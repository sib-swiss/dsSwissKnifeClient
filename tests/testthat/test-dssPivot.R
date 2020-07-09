test_that("dssSuggestPivot  works", {
  session1$iris1 <-session1$iris
  session1$iris1$id <- 1:nrow(session1$iris1)
  session1$iris_melt <- suppressWarnings(reshape2::melt(session1$iris1,'id' ))
  x <- dssSuggestPivot('iris_melt', by.col = 'id', datasources = opals["server1"])
  expect_equal(x$server1, "id ~ variable")

})

test_that("dssPivot  works", {
  dssPivot('wide_iris', 'iris_melt', 'value', by.col = 'id', datasources = opals["server1"], async = FALSE)
  session1$new_iris <- session1$wide_iris[,c(4,5,2,3,6,1)]
  colnames(session1$new_iris) <- colnames(session1$iris1)
 x <-  data.frame(lapply(session1$new_iris, as.character ), stringsAsFactors = FALSE)
 y <-  data.frame(lapply(session1$iris1, as.character ), stringsAsFactors = FALSE)
    expect_equal(x, y)
})
