% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mockSummarisedResult.R
\name{mockSummarisedResult}
\alias{mockSummarisedResult}
\title{Function to create a mock summarised result tibble.}
\usage{
mockSummarisedResult(
  seed = 1,
  startDate = as.Date("2021-01-01"),
  endDate = as.Date("2021-12-31"),
  populationSize = 10
)
}
\arguments{
\item{seed}{Seed value for the random mock data generated}

\item{startDate}{Start date for the mock data}

\item{endDate}{End date for the mock data}

\item{populationSize}{choose size of the population}
}
\description{
Function to create a mock summarised result tibble.
}
\examples{
\donttest{
library(ggoxford)

mockSummarisedResult(seed = 1,as.Date("2021-01-01"),as.Date("2021-12-31"))
}

}
