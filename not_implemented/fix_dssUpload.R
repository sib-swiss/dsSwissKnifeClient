library(uwot)
library(dsSwissKnifeClient)
myUpload <- dssUpload
.splitInEqualChunks <- dsSwissKnifeClient:::.splitInEqualChunks
x <- load_uwot('/home/iulian/Downloads/umapmod_Male')
x <- readBin('/home/iulian/Downloads/umapmod_Male', 'raw', n=file.size(('/home/iulian/Downloads/umapmod_Male')))
x1 <-dsSwissKnifeClient:::.encode.arg(x, serialize.it = TRUE)
system.time(z <- .splitInEqualChunks(x1, 1e+06))
z1 <- paste(z, collapse='')
y1 <- dsSwissKnife:::.decode.arg(z1)
writeBin(y1, '/home/iulian/Downloads/umapmod_Male2')
uw <- load_uwot('/home/iulian/Downloads/umapmod_Male2')
# so this works


setwd('/mnt/shareddisk/Sophia/jamboree_2022/')#
source('./login.R')

# build a login data frame
builder <- newDSLoginBuilder()
builder$append(server='sib', url= 'https://sophia-fdb.vital-it.ch/sib',
               user = getOption('datashield.username'), password = getOption('datashield.password'), driver = "OpalDriver")
logindata <- builder$build()

# login
opals <- datashield.login(logindata)
#myUpload('x', maxsize=1e+07) # not working

dssUwot('umap_transform')
