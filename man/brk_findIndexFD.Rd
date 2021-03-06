% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brk_FunctionalData.R
\name{brk_findIndexFD}
\alias{brk_findIndexFD}
\title{find index}
\usage{
brk_findIndexFD(sf, key, value)
}
\arguments{
\item{sf}{An object of class \link[sf]{sf}}

\item{key}{character string. name of the column to select}

\item{value}{value of the element to return index from the column defined by key}
}
\value{
vector if not all index are equal. scalar if all equal.
}
\description{
find index
}
