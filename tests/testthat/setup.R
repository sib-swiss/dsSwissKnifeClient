datashield.aggregate(opals['local1'], as.symbol('partialData("iris", 1, 40)'))
pkgload::load_all('/home/iulian/datashield/DSOpal')


options(verbose=FALSE)

options(opal.username='administrator',
        opal.password='password',

        opal.url='http://192.168.2.104:8080'
)

builder <- newDSLoginBuilder()
builder$append(server="sim1", url=getOption("opal.url"), table="datashield.CNSIM",
               user=getOption("opal.username"), password=getOption("opal.password"))

logindata <- builder$build()


opals <- DSOpal::datashield.login(logins = logindata, assign = TRUE)
