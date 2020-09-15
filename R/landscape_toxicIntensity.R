# toxicIntensity.R 
# Part of the briskaR package.
#
# Copyright (C) 2015        Virgile Baudrot <virgile.baudrot@inra.fr> 
#                           Melen Leclerc <melen.leclerc@inra.fr>
#                           Jean-Francois Rey <jean-francois.rey@inra.fr>
#                           Samuel Soubeyrand <Samuel.Soubeyrand@inra.fr>
#                           Emily Walker <emily.walker@inra.fr>
#                           INRA - BioSP Site Agroparc - 84914 Avignon Cedex 9
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#


#' @title toxicIntendity function wrapping dispersal and exposure
#' 
#' @name toxicIntensity
#' 
#' @param object sf or SpatialPolygonsDataFrame. A simple feature of class \link[sf]{sf} or \link[sp]{SpatialPolygonsDataFrame}
#' @param sf sf. And object of class `sf` on which exposure is computed from the previous
#' list of raster by patch `RasterStack_dispersal`. See \link[sf]{sf} for details.
#' @param size_raster integer. Raster size (default = 2^10)
#' @param tolerance_square numeric. Tolerance rate to test if an sf set is squared
#' @param kernel string. Dispersion kernel, function name (default = NIG)
#' @param kernel.options list. Parameters list for the kernel function
#' @param loss numeric. Numeric vector to applied a loss on exposure cells.
#' @param beta numeric. toxic adherence parameter between 0 and 1 (default = 0.4).
#' @param nbr_cores integer. Parameters for parallel computing: the
#' number of cores to use, i.e. at most how many child processes
#' will be run simultaneously. Default is \code{1} (non parallel).
#' @param squared_frame sf. Select the sf to be considered as frame to rasterized.
#' Default is `NULL`, and `object` is used. 
#' @param quiet boolean. Set `TRUE` to remove progress bar.
#' 
#' @details The dispersal of contaminants is implemented by rastering the landscape and
#'  by computing the convolution between sources emissions and a dispersal kernel.
#' 
#' The dispersion kernel by default is Normal Inverse Gaussian kernel ("NIG" function). 
#' Currently, two others are implemented "geometric" (with parameter \code{a}) and "2Dt" kernels 
#' (with parameters \code{a}, \code{b}, \code{c1}, \code{c2}).
#' 
#' Local intensity depends of \code{beta} and \code{alpha} parameters. Beta represents the toxic adherence between [0,1].
#' Alpha represents a list of parameters of the lost of toxic particules due to covariates (precipitation).
#' There are two configurations to integrate the loss in the function : 
#' (i) simulating covariate (simulate=TRUE) or (ii) uploading covariate (simulate=FALSE).
#' The covariate is linked to the loss by a linear regression with paramaters minalpha, maxalpha, covariate_threshold.
#' 
#' @export
#' 
toxicIntensity <- function(object,
                           sf,
                           size_raster = 2^10,
                           tolerance_square = 0.1,
                           kernel = "NIG",
                           kernel.options = list(
                             "a1" = 0.2073 ,
                             "a2" = 0.2073 ,
                             "b1" = 0.3971,
                             "b2" = 0.3971,
                             "b3" = 0.0649,
                             "theta" = 0
                           ),
                           loss = NULL,
                           beta = 0.4,
                           nbr_cores = 1,
                           squared_frame = NULL,
                           quiet=FALSE){
  
  
  message("Compute dispersal... ", appendLF = TRUE)
  
  RasterStack_dispersal <- brk_dispersal( object,
                                          size_raster,
                                          tolerance_square,
                                          kernel,
                                          kernel.options,
                                          nbr_cores,
                                          squared_frame)
  
  message("done", appendLF = TRUE)
  message("Compute dispersal... ", appendLF = TRUE)
  
  RasterStack_exposure <- brk_exposure(RasterStack_dispersal,
                                       sf,
                                       loss,
                                       beta,
                                       nbr_cores,
                                       quiet)
  message("done", appendLF = TRUE)
  
  
  return(RasterStack_exposure)
  
}

# plot toxicIntensity landscape
# 
#

