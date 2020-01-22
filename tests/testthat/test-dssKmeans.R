test_that("dssKmeans works", {
  my_kmeans <- dssKmeans('iris', centers = 3, iter.max =30, nstart = 30, type = 'combine', datasources = opals)
  data("iris", envir = environment())
  local.kmeans <- kmeans(iris[,1:4], centers = 3, iter.max = 30, nstart = 30, algorithm = 'Forgy')
  my_kmeans$global$centers
   local.kmeans$centers

  x <- apply(my_kmeans$global$centers,1, function(z){
   print(z)
    out <- apply(local.kmeans$centers,1, function(y) abs(y-z))
    print(Reduce(function(a,b){
      if (all(a >= b)){
        return(b)
      }
      return(a)
    },as.list(out)))
  })
  local.kmeans$centers[1,] - my_kmeans$global$centers[1,]
  expect_e(orig[[1]])
})
