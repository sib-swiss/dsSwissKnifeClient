#pkgload::load_all('/home/iulian/datashield/DSI')
#pkgload::load_all('/home/iulian/datashield/DSOpal')
library(DSLite)
pkgload::load_all('/home/iulian/datashield/dsSwissKnife')
#pkgload::load_all('~/Documents/data/dsSwissKnife/dsSwissKnife')
dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
dslite.server2 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))

#library(DSI)
#library(dsBaseClient)



builder <- newDSLoginBuilder()
builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
builder$append(server="server2", url='dslite.server2',driver = "DSLiteDriver")

logindata <- builder$build()


opals <<- datashield.login(logins = logindata, assign = TRUE)

session1 <- dslite.server1$getSession(dslite.server1$getSessionIds()) #only local
session2 <- dslite.server2$getSession(dslite.server2$getSessionIds()) #only local

datashield.aggregate(opals['server1'], as.symbol('partialData("iris", 1, 40)')) #only local
datashield.aggregate(opals['server2'], as.symbol('partialData("iris", 41, 150)')) #only local

# library(GRridge)
# datashield.aggregate(opals['server1'], as.symbol('fullData("dataFarkas", "GRridge")') ) #only local

data("iris")
idx1 <<- sample(150, 100) # for randomforests
idx2 <<- sample(150, 100)
set.seed(1234)

library(keras)
library(dsSwissKnife)

tensorflow::install_tensorflow(version='cpu')
tensorflow::tf_config()


n <- dim(iris)[1]
iris <- iris[sample(n),]

nr_classes = nlevels(iris$Species)

model <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = "relu", input_shape = 4) %>%
  layer_dropout(0.2) %>%
  #  layer_dense(5, activation = "relu") %>%
  layer_dense(nr_classes, activation = "softmax")


json.m <- model_to_json(model)
#
weights <- dssTfNNFit("iris", json.m, cl.labels = "Species", list(loss = 'categorical_crossentropy', optimizer = NULL, metrics = 'accuracy'),
                   list(epochs = 50, batch_size = 8, validation_split = 0.2))





