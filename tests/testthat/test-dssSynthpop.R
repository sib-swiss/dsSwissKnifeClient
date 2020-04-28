test_that("dssSynthpop works", {
  datashield.aggregate(opals["local1"], as.symbol('fullData("SD2011", "synthpop")') )
  dssSubset('SD', 'SD2011', col.filter = 'c(5:12)', datasources = opals['local1'])

  dt  <- dssSynthpop('SD', seed=123, datasources = opals['local1'])
  expect_lt(mean(dt$local1$syn$income, na.rm=TRUE) - 1389.617, .001)
})
