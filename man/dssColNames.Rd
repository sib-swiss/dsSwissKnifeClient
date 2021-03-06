% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dssColNames.R
\name{dssColNames}
\alias{dssColNames}
\title{Get or set the column names of a dataframe}
\usage{
dssColNames(
  df,
  value = NULL,
  to.replace = NULL,
  async = TRUE,
  datasources = NULL
)
}
\arguments{
\item{df}{a character, name of the dataframe}

\item{value}{a vector containing the new column names (if null, the function simply returns the colnames)}

\item{to.replace}{a vector containing the names of the columns to replace. It must have the same length as the value vector. If null, all column names will be replaced.}

\item{async}{same as in datashield.assign}

\item{datasources}{same as in datashield.assign}
}
\value{
the column names of the dataframe
}
\description{
Unlike ds.colnames this function allows to set the names too
}
\examples{
# open a local pseudo connection:
library(DSLite)
dslite.server1 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
dslite.server2 <<- newDSLiteServer(config = defaultDSConfiguration(include=c('dsSwissKnife')))
builder <- newDSLoginBuilder()
builder$append(server="server1", url='dslite.server1',driver = "DSLiteDriver")
builder$append(server="server2", url='dslite.server2',driver = "DSLiteDriver")
logindata <- builder$build()
opals <- datashield.login(logins = logindata, assign = TRUE)
# load the iris dataset
datashield.aggregate(opals[1], as.symbol('partialData("iris", 1, 70)'))
datashield.aggregate(opals[2], as.symbol('partialData("iris", 71, 150)'))
dssColNames('iris')
# set them to something else:
dssColNames('iris', value =  c('sl', 'sw', 'pl', 'pw', 'sp'))
#check again:
dssColNames('iris')
}
