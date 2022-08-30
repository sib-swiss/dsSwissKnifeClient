test_that("dssKmeans works", {
  set.seed(1234)
  my_kmeans <- dssKmeans('iris', centers = 3, iter.max =50, nstart = 100, type = 'combine', membership_suffix = 'test_km_3')
  data("iris", envir = environment())
  local.kmeans <- kmeans(iris[,1:4], centers = 3, iter.max = 50, nstart = 100, algorithm = 'Forgy')
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
  expect_true(all(mins < 1e-02))
})
