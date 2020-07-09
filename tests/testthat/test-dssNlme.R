test_that("dssNlme_groupedData works", {
  datashield.aggregate(opals["server1"], as.symbol('partialData("Orthodont", NULL, NULL, "nlme")'))
  dssNlme_groupedData(newobj = 'grouped', formula =  distance ~ age | Subject, data ='Orthodont', async = FALSE, datasources = opals["server1"] )
  expect_true(suppressWarnings('grouped' %in% datashield.symbols(opals["server1"])[[1]]))
  lme.model <- dssNlme_lme(fixed = distance ~ age, data = 'grouped', random = ~ 1, async = FALSE, datasources = opals["server1"])
  expect_lte(lme.model$server1$sigma - 1.431592, 1e-03)
})
