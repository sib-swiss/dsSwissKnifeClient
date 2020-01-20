test_that("dssClogit works", {
  datashield.aggregate(opals[1], as.symbol('partialData("infert")'))

  clogit.model <- dssClogit(formula = case ~ spontaneous + induced + stratum, data='infert', datasources = opals[1])
  data('infert', envir = environment())
  library(survival)
  clogit.local <- survival::clogit(case ~ spontaneous + induced + stratum, data=infert)
  expect_true(all(clogit.local$coefficients == clogit.model$local1$coefficients))
})
