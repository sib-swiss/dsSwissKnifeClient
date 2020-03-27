test_that("dssSuggestPivot  works", {
  opals$local1$envir$iris1 <-opals$local1$envir$iris
  opals$local1$envir$iris1$id <- 1:nrow(opals$local1$envir$iris1)
  opals$local1$envir$iris_melt <- suppressWarnings(reshape2::melt(opals$local1$envir$iris1,'id' ))
  x <- dssSuggestPivot('iris_melt', by.col = 'id', datasources = opals["local1"])
  expect_equal(x$local1, "id ~ variable")

})

test_that("dssPivot  works", {
  dssPivot('wide_iris', 'iris_melt', 'value', by.col = 'id', datasources = opals["local1"])
  opals$local1$envir$new_iris <- opals$local1$envir$wide_iris[,c(4,5,2,3,6,1)]
  colnames(opals$local1$envir$new_iris) <- colnames(opals$local1$envir$iris1)
 x <-  data.frame(lapply(opals$local1$envir$new_iris, as.character ), stringsAsFactors = FALSE)
 y <-  data.frame(lapply(opals$local1$envir$iris1, as.character ), stringsAsFactors = FALSE)
    expect_equal(x, y)
})
