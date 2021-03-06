% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brk_dispersal.R
\name{brk_dispersal}
\alias{brk_dispersal}
\title{Compute dispersal}
\usage{
brk_dispersal(
  object,
  size_raster,
  tolerance_square,
  kernel,
  kernel.options,
  nbr_cores,
  squared_frame
)
}
\arguments{
\item{object}{sf or patialPolygonsDataFrame. A simple feature of class \link[sf]{sf} or \link[sp]{SpatialPolygonsDataFrame}}

\item{size_raster}{integer. Raster size (default = 2^10)}

\item{tolerance_square}{numeric. Tolerance rate to test if an sf set is squared}

\item{kernel}{string. Dispersion kernel, function name (default = NIG)}

\item{kernel.options}{list. Parameters list for the kernel function}

\item{nbr_cores}{integer. Parameters for parallel computing: the
number of cores to use, i.e. at most how many child processes
will be run simultaneously. Default is \code{1} (non parallel).}

\item{squared_frame}{sf. Select the sf to be considered as frame to rasterized.
Default is `NULL`, and `object` is used.}
}
\description{
Simulate contaminants or individuals frequency over the landscape by two
steps: dispersal of elements and local intensity/frequency of elements after dispersal.
}
\details{
The dispersal of contaminants or individuals is implemented by rastering
the landscape and by computing the convolution between sources emissions
and a dispersal kernel.

The dispersion kernel by default is Normal Inverse Gaussian kernel
 ("NIG" function). Currently, two others are implemented "geometric"
 (with parameter \code{a}) and "2Dt" kernels (with parameters \code{a},
 \code{b}, \code{c1}, \code{c2}).
}
