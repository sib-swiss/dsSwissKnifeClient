test_that("dssLmResid works with global coefficients", {
  indvars <- colnames(iris)[1:4]
  deps <- indvars[c(3,2)]
  resid <- list()
  outc <- sapply(deps, function(x){
    formula <- paste0(x, ' ~ ', paste(setdiff(indvars, x), collapse = ' + '))
    print(formula)
    md <- lm(formula, data = iris[, unique(c(x, indvars))])
    resid[[x]] <<- scale(unname(md$residuals))
    md$coefficients
  }, simplify = FALSE)
  dssLmResid(outc, indvars, 'iris')
  dssScale( 'scaledres', 'residuals')
  tot_remote <- rbind(session1$scaledres, session2$scaledres)
  tot_loc <- Reduce(rbind, resid)
  smalls <- abs(tot_loc - tot_remote)
  expect_true(any(sapply(smalls, max) < 1e-03))
})


test_that("dssLmResid works with local models", {

  iris <- iris[1:75,]
  indvars <- colnames(iris)[1:4]
  outc <- indvars[c(3,2)]
  dssLmResid(outc, indvars, 'iris', datasources = opals[1])
  dssScale( 'scaledres', 'residuals', datasources = opals[1])
  loc_res <- sapply(outc, function(x){
    formula <- paste0(x, ' ~ ', paste(setdiff(indvars, x), collapse = ' + '))
    lm(formula, data = iris)$residuals
  })
  loc_sc <- scale(loc_res)
  smalls <- abs(loc_sc - session1$scaledres)
  expect_true(any(sapply(smalls, max) < 1e-05))
})
