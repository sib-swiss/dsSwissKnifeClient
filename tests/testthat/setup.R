#pkgload::load_all('/home/iulian/datashield/DSI')
#pkgload::load_all('/home/iulian/datashield/DSOpal')
pkgload::load_all('/home/iulian/datashield/DSLite')
pkgload::load_all('/home/iulian/datashield/dsSwissKnife')
dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
dslite.server2 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))

#library(DSI)
#library(dsBaseClient)



builder <- newDSLoginBuilder()
builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
builder$append(server="server2", url='dslite.server2',driver = "DSLiteDriver")

logindata <- builder$build()


opals <<- datashield.login(logins = logindata, assign = TRUE)
session1 <- dslite.server1$getSession(dslite.server1$getSessionIds())
session2 <- dslite.server2$getSession(dslite.server2$getSessionIds())
datashield.aggregate(opals['server1'], as.symbol('partialData("iris", 1, 40)'))
datashield.aggregate(opals['server2'], as.symbol('partialData("iris", 41, 150)'))
datashield.aggregate(opals['server1'], as.symbol('fullData("dataFarkas", "GRridge")') )
data("iris")
idx1 <<- sample(150, 100) # for randomforests
idx2 <<- sample(150, 100)
set.seed(1234)




