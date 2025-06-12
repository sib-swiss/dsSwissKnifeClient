test_that("dssMelt works", {

  dssMelt('iris_long', data = 'iris', measurement.vars = c('Sepal.Length', 'Sepal.Width', 'Petal.Length, Petal.Width'), variable.name = 'measurement_name' , datasources = opals['server1'])

  expect_true(is.numeric(n$server1))

})

