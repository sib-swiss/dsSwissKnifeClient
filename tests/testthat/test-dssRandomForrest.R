test_that("Classification works (discrete y).", {

  # We know these are "setosa" because of their small Petal.Width
  testData = data.frame(
    Sepal.Length = c(5.5, 6.1),
    Sepal.Width = c(3.0, 3.1),
    Petal.Length = c(1.5, 1.4),
    Petal.Width = c(0.2, 0.19)
  )

  # On each server node, choose 100 flowers among 150 to be the training data
  set.seed(1234)


  #datashield.assign(opals, '_', quote(set.seed(1234)))
#  datashield.assign(opals, 'idx1', quote(sample(150, 100)))
 # datashield.assign(opals, 'idx2', quote(sample(150, 100)))

  # Assign a fraction to the iris data to the "subiris" variable name on each node
  datashield.aggregate(opals, as.symbol('fullData("iris")'))
#  dssSubset("subiris", "iris", row.filter = idx1, datasources = opals["server1"])
 # dssSubset("subiris", "iris", row.filter = idx2, datasources = opals["server2"], async = FALSE)

  dssSubset("subiris", "iris", row.filter = 'sample(150,100)', datasources = opals)
  dssSubset("subiris_train", "subiris", row.filter = 'sample(100,90)', datasources = opals)
  dssSubset("subiris_test", "subiris", row.filter = '!(rownames(subiris) %in% rownames(subiris_train))', datasources = opals)

  # Run `forest` - in sync mode for debugging
  dep_var = "Species"
  expl_vars = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
  # reset iris for the other tests:
  datashield.aggregate(opals["server1"], as.symbol('partialData("iris", 1, 40)'))
  datashield.aggregate(opals["server2"], as.symbol('partialData("iris", 41, 150)'), async=FALSE)
 # result = dssRandomForest('subiris', dep_var, expl_vars, testData, async = FALSE,datasources = opals)
  train_args <- list('what' = 'subiris_train' , dep_var = dep_var, expl_vars = expl_vars, nodesize = 5)
  result = dssRandomForest(train = train_args, async = FALSE,datasources = opals)
  test_args <- list(forest = result, testData = 'subiris_test')
  prediction = dssRandomForest(train = NULL,test = test_args, async = FALSE,datasources = opals)
  local_prediction <- dssRandomForest(train = NULL, test = list(result, testData))
  expect_identical(local_prediction, c('setosa', 'setosa'))

})


test_that("Regression works (continuous y).", {


  # These ones are setosa based on the small Petal.Width,
  # and should be predicted to have small Sepal.Length (< 5).
  testData = data.frame(
    Sepal.Width = c(3.0, 3.1),
    Petal.Length = c(1.5, 1.4),
    Petal.Width = c(0.2, 0.19)
  )

  # On each server node, choose 100 flowers among 150 to be the training data


  ##datashield.assign(opals, '_', quote(set.seed(1234)))
  #datashield.assign(opals, 'idx1', quote(sample(150, 100)))
  #datashield.assign(opals, 'idx2', quote(sample(150, 100)))

  # Assign a fraction to the iris data to the "subiris" variable name on each node
  datashield.aggregate(opals, as.symbol('fullData("iris")'))
 # dssSubset("subiris", "iris", row.filter = "idx1", datasources = opals["server1"])
#  dssSubset("subiris", "iris", row.filter = "idx2", datasources = opals["server2"])
  dssSubset("subiris", "iris", row.filter = 'sample(150,100)', datasources = opals)
  dssSubset("subiris_train", "subiris", row.filter = 'sample(100,90)', datasources = opals)
  dssSubset("subiris_test", "subiris", row.filter = '!(rownames(subiris) %in% rownames(subiris_train))', datasources = opals)

  # Run `forest` - in sync mode for debugging
  dep_var = "Sepal.Length"
  expl_vars = c("Sepal.Width","Petal.Length","Petal.Width")


  #result = dssRandomForest('subiris', dep_var, expl_vars, testData, async = FALSE, datasources = opals)
  train_args <- list('what' = 'subiris_train' , dep_var = dep_var, expl_vars = expl_vars, nodesize = 5)
  result = dssRandomForest(train_args, async = FALSE, datasources = opals)
  test_args <- list(result, testData)
  prediction <- dssRandomForest(NULL, test_args)
  # reset iris for the other tests:
  datashield.aggregate(opals["server1"], as.symbol('partialData("iris", 1, 40)'))
  datashield.aggregate(opals["server2"], as.symbol('partialData("iris", 41, 150)'))
  p = prediction
  expect_lte(p[1], 5.0)
  expect_lte(p[2], 5.0)
})


test_that("It still works when one of the nodes is missing a category.", {
  dep_var = "Species"
  expl_vars = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")

  testData = data.frame(
    Sepal.Length = c(5.5, 6.1),
    Sepal.Width = c(3.0, 3.1),
    Petal.Length = c(1.5, 1.4),
    Petal.Width = c(0.2, 0.19)
  )
  df1 = iris[1:70,]  # only "setosa" and "versicolor"
  df2 = iris[71:150,]  # "versicolor" and "virginica"
  df1[,5] = droplevels(df1[,5])
  df2[,5] = droplevels(df2[,5])

  # What dsCDISC::forestDSS does
  ntree1 = max(min(10, nrow(df1)), 150)
  forest1 = randomForest::randomForest(df1[,expl_vars], y = df1[,dep_var], nodesize=5, norm.votes = TRUE, ntree = ntree1)
  ntree2 = max(min(10, nrow(df2)), 150)
  forest2 = randomForest::randomForest(df2[,expl_vars], y = df2[,dep_var], nodesize=5, norm.votes = TRUE, ntree = ntree2)
  forests = list(forest1, forest2)

  # Do the individual predictions locally, just to try
  pred1 = predict(forest1, testData)
  pred2 = predict(forest2, testData)
  # Predicts setosa correctly
  expect_equal(as.character(pred1), c("setosa", "setosa"))
  # Cannot predict setosa because it does not know it, so predicts the next closest
  expect_equal(as.character(pred2), c("versicolor", "versicolor"))

  # Use .predict to merge the predictions
  pred = dsSwissKnifeClient::dssRandomForest(NULL,list(forests, testData))
  # Because the second partial prediction has slightly more votes, the merged prediction is off!
  # The node that does not know about setosa puts all its 150 votes into the wrong category,
  #  while the one that does know setosa has 2 misclassified votes, so by
  #  150 votes vs 148, the answer is versicolor instead of setosa.
  # This is a - weird - example showing that merging the forests is actually not the same as
  # using a forest built on the whole dataset.
  expect_equal(as.character(pred), c("versicolor", "versicolor"))
})


