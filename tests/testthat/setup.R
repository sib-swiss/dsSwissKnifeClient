pkgload::load_all('/home/iulian/datashield/dsSwissKnife')
x <- dssCreateFakeServers('test', servers = 2)
opals <- datashield.login(x)

datashield.aggregate(opals['local1'], as.symbol('partialData("iris", 1, 40)'))
datashield.aggregate(opals['local2'], as.symbol('partialData("iris", 41, 150)'))
data("iris")
part_iris_1 <- test$locals$local1$envir$iris
part_iris_2 <- test$locals$local2$envir$iris

#logindata <- read.delim('/home/iulian/datashield/logindata_test.txt')
#opals <- opal::datashield.login(logindata)
#datashield.aggregate(opals['local1'], as.symbol('partialData("iris", 1, 40)'))
#datashield.aggregate(opals['local2'], as.symbol('partialData("iris", 41, 150)'))

