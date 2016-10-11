# Class-Landscape.R 
# Part of the briskaR package.
#
# Copyright (C) 2015        Melen Leclerc <melen.leclerc@inra.fr>
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

#' @title Class Landscape
#'
#' @description Class \code{Landscape} defines a landscape
#'
#' @details \code{Landscape} objects can be created by calling of the allocator new("Landscape", ...), or (preferred) by calling to the function \link{simulateLandscape} or \link{loadLandscape}.
#'
#' @seealso \link{simulateLandscape} , \link{loadLandscape} , \link{loadLandscapeSIG}
#'   
#' @name Landscape
#' @rdname Landscape-class
#' @slot thelandscape A SpatialPolygonsDataFrame
#' @slot xmin Left x-axis coordinate (in meters)
#' @slot xmax Right x-axis coordinate (in meters)
#' @slot ymin Bottom y-axis coordinate (in meters)
#' @slot ymax Top y-axis coordinate (in meters)
#' @slot n Number of fields in the landscape
#' @aliases Landscape-class
#'
#' @import methods
#' @import stats
#' @import rgeos
#' @import sp
#' @importFrom grDevices colorRampPalette heat.colors rgb
#' @importFrom graphics legend lines mtext par points rect title
#' @importFrom fields Exponential
#' @importFrom fields image.plot
#' @importFrom MASS mvrnorm
#' @importFrom deldir deldir
#' @importFrom deldir tile.list
#' @importFrom rgdal readOGR
#' @importFrom raster crs<-
#' @importFrom raster image
#' 
#' @include 0briskaR.R
#'  
#' @exportClass Landscape
setClass(
  Class="Landscape",
  slots=c(thelandscape="SpatialPolygonsDataFrame",
          xmin="numeric",
          xmax="numeric",
          ymin="numeric",
          ymax="numeric",
          n="numeric"),
  prototype=list(thelandscape=new(Class="SpatialPolygonsDataFrame"),
                 xmin=0,
                 xmax=5000,
                 ymin=0,
                 ymax=5000,
                 n=1
                ),
  validity=function(object) {
    if( xmin >= xmax | ymin >= ymax) {
      stop("ERROR : [Landscape:validity] landscape coordinates error")
    }
    return(TRUE)
  }
  )

