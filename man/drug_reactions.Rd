% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_reaction_node_parser.R
\name{drug_reactions}
\alias{drug_reactions}
\title{Drug Reactions Parsers}
\usage{
drug_reactions()
}
\value{
a tibble with 5 variables:
\describe{
  \item{sequence}{	Reactions are displayed within a numerical sequence}
  \item{left_drugbank_name}{The substrate of the reaction. Maybe a drug or a
   metabolite.}
  \item{rightt_drugbank_name}{	The product of the reaction. Maybe a drug or a
   metabolite.}
  \item{left_drugbank_id}{}
  \item{right_drugbank_id}{}
  \item{parent_id}{drugbank id}
}
}
\description{
Extract the sequential representation of the metabolic reactions that this
 drug molecule is involved in. Depending on available information, this may
 include metabolizing enzymes, reaction type, substrates, products,
 pharmacological activity of metabolites, and a structural representation of
  the biochemical reactions.
}
\keyword{internal}
