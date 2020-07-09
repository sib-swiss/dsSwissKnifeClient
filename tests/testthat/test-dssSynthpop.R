test_that("dssSynthpop works", {
  datashield.aggregate(opals["server1"], as.symbol('fullData("SD2011", "synthpop")') )
  dssSubset('SD', 'SD2011', col.filter = 'c(5:12)', datasources = opals['server1'])

  dt  <- dssSynthpop('SD', seed=123, datasources = opals['server1'])
  expect_lt(mean(dt$server1$syn$income, na.rm=TRUE) - 1389.617, .001)
})
