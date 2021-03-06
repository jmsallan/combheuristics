% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LocalSearchFSFunctions.R
\name{PalmerTrapezes}
\alias{PalmerTrapezes}
\title{Palmer and trapezes heuristics}
\usage{
PalmerTrapezes(M)
}
\arguments{
\item{M}{A matrix object storing the time to process task j (column) in machine i (row).}
}
\value{
pal A vector with the positions of the solution of the Palmer heuristic.

tra A vector with the positions of the solution of the trapezes heuristic.
}
\description{
Palmer and Trapezes heuristics for the permutative flowshop problem.
}
\examples{
set.seed(2020)
instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
PalmerTrapezes(instance)
}
