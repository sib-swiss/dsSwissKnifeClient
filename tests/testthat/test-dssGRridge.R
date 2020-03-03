test_that("CreatePartition works", {
  dssGRridge('CreatePartition', 'CpGannFarkas', newobj = 'firstPart', datasources = opals[1])

})


test_that("CreatePartition works", {
  x <- dssGRridge('grridge', 'datcenFarkas', 'respFarkas','firstPart' , datasources = opals[1])

})
