% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brk_FunctionalData.R
\name{brk_cFilterFD}
\alias{brk_cFilterFD}
\alias{brk_cFilterFD_}
\alias{brk_cFilterFD2}
\alias{brk_cFilterFD3}
\title{Function used to filter functional data in \link[sf]{sf} objects.}
\usage{
brk_cFilterFD(sf, key, index)

brk_cFilterFD_(sf, key, index)

brk_cFilterFD2(sf, key1, key2, index)

brk_cFilterFD3(sf, key1, key2, key3, index)
}
\arguments{
\item{sf}{sf. An object of class \link[sf]{sf}}

\item{key}{character string. The name of the column to select}

\item{index}{integer (or vector). The index of the functional data.}

\item{key1}{character string. The name of the column to select}

\item{key2}{character string. The name of the column to select}

\item{key3}{character string. The name of the column to select}
}
\description{
Function used to filter functional data in \link[sf]{sf} objects.
}
