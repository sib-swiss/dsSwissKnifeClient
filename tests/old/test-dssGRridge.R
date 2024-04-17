test_that("dssGRridge works", {
  dssGRridge('CreatePartition', 'CpGannFarkas', newobj = 'firstPart', async = FALSE, datasources = opals["server1"])
 # s <- dsBaseClient::ds.length('firstPart', datasources = opals["server1"])
  s <- length(session1$firstPart)
  expect_equal(s ,6)

  x <- dssGRridge('grridge', 'datcenFarkas', 'respFarkas','firstPart' , async = FALSE, datasources = opals["server1"])
  expect_equal(levels(x$server1$true), c("Normal", "Precursor"))
})
