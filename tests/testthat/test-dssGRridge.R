test_that("dssGRridge works", {
  dssGRridge('CreatePartition', 'CpGannFarkas', newobj = 'firstPart', async = FALSE, datasources = opals["local1"])
  s <- dsBaseClient::ds.length('firstPart', datasources = opals["local1"])
  expect_equal(s[[1]] ,6)

  x <- dssGRridge('grridge', 'datcenFarkas', 'respFarkas','firstPart' , async = FALSE, datasources = opals["local1"])
  expect_equal(levels(x$local1$true), c("Normal", "Precursor"))
})
