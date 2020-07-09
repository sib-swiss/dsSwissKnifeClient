test_that("Bioconductor install works", {
  x <-  datashield.aggregate(opals["server1"], quote(biocInstall('GRridge')), async = FALSE)
  expect_equal(x, x)

})
