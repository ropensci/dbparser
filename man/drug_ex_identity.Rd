% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_parsers.R
\name{drug_ex_identity}
\alias{drug_ex_identity}
\title{Drug External Identifiers parser}
\usage{
drug_ex_identity()
}
\value{
a tibble with the following variables:
\describe{
 \item{resource}{Name of the source database.}
 \item{identifier}{Identifier for this drug in the given resource.}
 \item{\emph{drugbank_id}}{drugbank id}
}
}
\description{
Identifiers used in other websites or databases providing information about
this drug.
}
\keyword{internal}
