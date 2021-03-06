% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brk_emission.R
\name{brk_emission}
\alias{brk_emission}
\alias{brk_emission_landscape}
\title{Wrapper function brk_emission}
\usage{
brk_emission(sf, keyTime, key, FUN)

brk_emission_landscape(
  sf,
  timeline = 1:61,
  emission = NULL,
  density = NULL,
  intensity = NULL,
  intensity_pmf = NULL,
  production = NULL
)
}
\arguments{
\item{sf}{A \link[sf]{sf} object}

\item{keyTime}{character. Name of the column for timeline. Used to check length of vectors in column vector object}

\item{key}{character. Name of the column which is going to be created}

\item{FUN}{A function applied on the sf object. See lapply functions for further details assuming \code{X = 1:nrow(sf)}, that is \code{sf[[key]] <- lapply(1:nrow(sf), FUN, ...)}.}

\item{timeline}{Vector of time units (e.g. days) covering all the function}

\item{emission}{Vector or Matrix given the quantity of emission per time unit per spatial unit.
Length of vector equal the length of the `timeline` vector (time unit matching).
Size of the matrix, `n,m`, is such as the number of row equal the number of sources in `sf` object, and the number of column
equals the length of the `timeline` vector.}

\item{density}{Scalar or Vector (with length equal to the number of sources in `sf` object) given the density
of the source(s) (e.g. number of plant by squared meter)}

\item{intensity}{Vector or Matrix given the quantity of emission per time unit per source.
Length of vector equal the length of the `timeline` vector (time unit matching).
Size of the matrix, `n,m`, is such as the number of row equal the number of sources in `sf` object, and the number of column
equals the length of the `timeline`.}

\item{intensity_pmf}{Vector or Matrix given distribution of emission along time (given a probability mass function with time) .
Length of vector equal the length of the `timeline` vector (time unit matching).
Size of the matrix, `n,m`, is such as the number of row equal the number of sources in `sf` object, and the number of column
equals the length of the `timeline`.}

\item{production}{Scalar or Vector (with length equal to the number of sources in `sf` object) total emission for one source (e.g. total number of grains by plant)}
}
\value{
A matrix indexed by sources ID (in rows) and by time ( in columns) whose rows give 
the values of intensity emission (number of grains) for every source.
}
\description{
`brk_emission` This function simulates emissions. Will simulate emissions shape in source fields of a landscape.

`brk_emission_landscape` Simulate sources emission intensity. With:
 - `emission` is the quantity of emission per time unit per spatial unit. With other argument, we have:
 - `emission = density x intensity = density x production x intensity_pmf`.
 - `density` is the density of the source, so the quantity of source per spatial unit.
 - `intensity` is the intensity of the emission for 1 source: that is the quantity of emission per time unit per source.
 - `production` is the overall production, the total emission, for one source unit: quantity of emission for the period
 - `intensity_pmf` is the distribution of emission along time. So we have `intensity = production x intensity_pmf`.
}
\details{
This function is a wrapper of with and lapply function and is like this: 
               sf[["key"]] <- with(sf, lapply(1:nrow(sf), FUN, ...))
 
 So, all column of sf can be called in FUN
}
