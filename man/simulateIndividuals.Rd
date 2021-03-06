% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Individuals_Methods.R
\name{simulateIndividuals}
\alias{simulateIndividuals}
\title{Wrapper function SimulateIndividuals}
\usage{
simulateIndividuals(
  sf,
  size = 100,
  timeline = 1:61,
  dob,
  life_duration,
  toxic_threshold
)
}
\arguments{
\item{sf}{A \link[sf]{sf} object}

\item{size}{Number of individuals to simulate}

\item{timeline}{Vector of the time line}

\item{dob}{A vector for the Date Of Birth of each individual between
\code{min} and \code{max} of the \code{timeline}.}

\item{life_duration}{A vector for the life duration of each individual}

\item{toxic_threshold}{A vector for the internal toxic threshold value leading to death for each individual}
}
\description{
This function simulates individuals as an Individuals object.

Will simulate \code{n} individuals in receptors fields of a landscape.
}
\details{
The Individuals object output includes for each individual the
coordinates, the date of birth, the life duration, the toxic threshold and
}
