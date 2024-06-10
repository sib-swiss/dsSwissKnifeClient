test_that("dssConsensusOPLS works", {
  datashield.assign(opals['server1'], 'X', as.symbol('demo_3_Omics[1:3]'))

  x <- dssConsensusOPLS( async = FALSE, datasources = opals["server1"], data = 'X', Y='demo_3_Omics$Y')
  expect_equal(names(x$server1), c("optimal","permuted", "permStats","plots"))
})
