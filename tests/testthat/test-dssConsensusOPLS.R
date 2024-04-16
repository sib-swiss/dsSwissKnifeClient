test_that("dssConsensusOPLS works", {
  datashield.assign(opals['server1'], 'X', as.symbol('demo_3_Omics[1:3]'))

  # s <- dsBaseClient::ds.length('firstPart', datasources = opals["server1"])
 # s <- length(session1$firstPart)
#  expect_equal(s ,6)

  x <- dssConsensusOPLS( async = FALSE, datasources = opals["server1"], data = 'X', Y='demo_3_Omics$Y', plots= TRUE)
  expect_equal(levels(x$server1$true), c("Normal", "Precursor"))
})
