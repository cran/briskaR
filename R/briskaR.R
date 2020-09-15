# briskaR-package.R 
# Part of the briskaR package.
#
# Copyright (C) 2015        Melen Leclerc <melen.leclerc@inrae.fr>
#                           Jean-Francois Rey <jean-francois.rey@inrae.fr>
#                           Samuel Soubeyrand <Samuel.Soubeyrand@inrae.fr>
#                           Emily Walker <emily.walker@inrae.fr>
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

#' @encoding UTF-8
#' @title Biological Risk Assessment
#' @description A spatio-temporal exposure-hazard model for assessing biological risk and impact.
#' The model is based on stochastic geometry for describing the landscape and the exposed individuals,
#' a dispersal kernel for the dissemination of contaminants and an ecotoxicological equation.
#' @aliases briskaR-package briskaR
#' 
#' @author Virgile Baudrot \email{virgile.baudrot@@posteo.fr}
#' @author Emily Walker \email{emily.walker@@inrae.fr}
#' @author Jean-Francois Rey \email{jean-francois.rey@@inrae.fr}
#' @author Melen Leclerc \email{melen.leclerc@@inrae.fr}
#' @author Samuel Soubeyrand \email{Samuel.Soubeyrand@@inrae.fr}
#' @author Marc Bourotte \email{marc.bourotte@@inrae.fr}
#' 
#' Maintainer: Jean-Francois REY \email{jean-francois.rey@@inrae.fr}
#' @docType package
#' @name briskaR-package
#' 
#' @details
#' The briskaR package contains functions and methods for quantifying spatio-temporal variation in contamination risk
#' around known polygon sources of contaminants, and quantifies the impact of the contaminants on the surrounding population of individuals
#' which are located in habitat areas and are susceptible to the contaminants.
#' 
#' The package implements an spatio-temporal exposure-hazard model based on (i) tools of stochastic geometry (marked polygon and point processes)
#' for structuring the landscape and describing the location of exposed individuals,
#' (ii) a method based on a dispersal kernel describing the dissemination of contaminant particles from polygon sources,
#' and (iii) ecotoxicological equations describing how contaminants affect individuals of the exposed population.
#'          
#' @keywords model spatial survival
#' 
#' @import sf
#' @import rgeos
#' 
#' 
"_PACKAGE"


#' @title Data set included in the package
#' @name data_brk
NULL

#' @rdname data_brk
#' @name maize_65
#' 
#' @docType data
#' @usage data(maize_65)
#' 
#' @param maize_65 A SpatialPolygonsDataFrame of class \link[sp]{SpatialPolygonsDataFrame}  defining a patchy landscape
#' 
NULL

#' @rdname data_brk
#' @name sfMaize65
#' 
#' @docType data
#' @usage data(sfMaize65)
#' 
#' @param sfMaize65  A set of MULTIPOLYGON of class \link[sf]{sf} defining a patchy landscape
#' 
NULL

#' @rdname data_brk
#' @name maize.emitted_pollen
#' 
#' @docType data
#' @usage data(maize.emitted_pollen)
#' 
#' @param maize.emitted_pollen A \link[base]{data.frame} of pollen emission  
#' 
NULL

#' @rdname data_brk
#' @name maize.proportion_pollen
#' 
#' @docType data
#' @usage data(maize.proportion_pollen)
#' 
#' @param maize.proportion_pollen A \link[base]{data.frame} of pollen proportion 
#' 
NULL

#' @rdname data_brk
#' @name Hofmann_2009
#' 
#' @docType data
#' @usage data(Hofmann_2009)
#' 
#' @param Hofmann_2009 A \link[base]{data.frame} of pollen emission
#' 
NULL

#' @rdname data_brk
#' @name Lang_2004
#' 
#' @docType data
#' @usage data(Lang_2004)
#' 
#' @param Lang_2004 A \link[base]{data.frame} of pollen emission
#' 
NULL

#' @rdname data_brk
#' @name Precipitation
#' 
#' @docType data
#' @usage data(Precipitation)
#' 
#' @param Precipitation   \link[base]{data.frame} of daily Precipitation from 01/01/2003 to 31/12/2013
#' 
#' 
NULL

#' @rdname data_brk
#' @name temperatureGermany
#' 
#' @docType data
#' @usage data(temperatureGermany)
#' 
#' @param temperatureGermany A \link[base]{data.frame} of temperature over a year in south and north of germany
#' 
NULL

#' @rdname data_brk
#' @name df_precipitation
#' 
#' @docType data
#' @usage data(df_precipitation)
#' 
#' @param df_precipitation A \link[base]{data.frame} of daily Precipitation from 01/01/2003 to 31/12/2013
#' 
NULL


# Add 'useDynLib(briskaR, .registration = TRUE)' in the NAMESPACE
#  
#' @useDynLib briskaR, .registration=TRUE
#' @import Rcpp
# @import RcppArmadillo
NULL
