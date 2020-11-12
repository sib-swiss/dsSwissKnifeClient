test_that("dssClogit works", {
  datashield.aggregate(opals["server1"], as.symbol('partialData("infert")'))

  clogit.model <- dssClogit(formula = case ~ spontaneous + induced + stratum, data='infert', datasources = opals["server1"])
  data('infert', envir = environment())
  library(survival)
  clogit.local <- survival::clogit(case ~ spontaneous + induced + stratum, data=infert)
  expect_identical(clogit.local$coefficients , clogit.model$server1$coefficients)
})
