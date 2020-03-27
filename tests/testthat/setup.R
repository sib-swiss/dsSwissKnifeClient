pkgload::load_all('/home/iulian/datashield/dsSwissKnife')
opals <- dssCreatePseudoServers(servers = 2)
#logindata <- read.delim('/home/iulian/datashield/logindata_test.txt')
#opals <- datashield.login(opals, logindata[2,,drop = FALSE])
#opals <- datashield.login(x)

datashield.aggregate(opals['local1'], as.symbol('partialData("iris", 1, 40)'))
datashield.aggregate(opals['local2'], as.symbol('partialData("iris", 41, 150)'))
datashield.aggregate(opals["local1"], as.symbol('fullData("dataFarkas", "GRridge")') )
data("iris")
part_iris_1 <- opals$local1$envir$iris
part_iris_2 <- opals$local2$envir$iris


#datashield.aggregate(opals['local1'], as.symbol('partialData("iris", 1, 40)'))
#datashield.aggregate(opals['local2'], as.symbol('partialData("iris", 41, 150)'))
#datashield.aggregate(opals["local1"], as.symbol('fullData("dataFarkas", "GRridge")') )

#x <- dssCreatePseudoServers(1)
#logindata <- read.delim('/home/iulian/datashield/logindata_test.txt')
#logindata  <- logindata[logindata$server == 'local2', ,drop = FALSE]
#logindata$server <- 'remote2'
#y <- datashield.login(logindata)
#opals <- c(y,x)
#datashield.aggregate(opals, as.symbol('partialData("iris", 1, 40)'), async = FALSE)
