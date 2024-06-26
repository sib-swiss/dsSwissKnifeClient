% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dssUpload.R
\name{dssUpload}
\alias{dssUpload}
\title{Upload an object}
\usage{
dssUpload(
  objname,
  maxsize = NULL,
  special.object.type = NULL,
  async = TRUE,
  datasources = NULL
)
}
\arguments{
\item{objname}{a character, the name of object to be uploaded}

\item{maxsize}{optional, an integer, the maximum size in bytes of the payload for one call. If set, the object will be sent over multibple
calls to the server function and rebuilt in the remote session(s)}

\item{special.object.type}{some objects need special treatment before and after transfer.
For the moment this is only implemented for uwot models (see library uwot) and the corresponding
value is 'uwot_model'}

\item{async}{a logical, see datashield.aggregate}

\item{datasources}{a list of opal objects obtained after logging into the opal servers (see datashield.login)}
}
\description{
Encode and upload a local object into the remote session(s). Optionally (for very large objects) the encoded stream
can be split and uploaded over multiple calls
}
