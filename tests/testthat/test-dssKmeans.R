test_that("dssKmeans works", {
  my_kmeans <- dssKmeans('iris', centers = 3, iter.max =30, nstart = 30, type = 'combine')
  data("iris", envir = environment())
  local.kmeans <- kmeans(iris[,1:4], centers = 3, iter.max = 30, nstart = 30, algorithm = 'Forgy')
# the order of the 2 matrices might be different so we need to contort like so:
  mins <- apply(my_kmeans$global$centers,1, function(z){
    out <- apply(local.kmeans$centers,1, function(y){
      abs(y-z)
    })
    ret <- Reduce(function(a,b){
      u <- out[,a]
      v <- out[,b]
      if(all(u <= v)){
        return(a)
      } else {
        return(b)
      }
      stop('Funky things happening, check the 2 center matrices.')
    },1:dim(out)[2])

  out[,ret]
  })
  expect_true(all(mins < 1e-05))
})
