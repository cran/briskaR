# pollen_data.R
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

data("maize.proportion_pollen", envir = environment())

#' @title Pollen sources emission simulation
#' 
#' @description Simulate pollen sources emission for maize crop. The proportion of plants
#' emitting pollen per day (during 12 days) was observed by Frédérique Angevin (Angevin et al. 2008).
#' 
#' @param sf A \link[sf]{sf} object defining sources fields
#' @param timeline Vector of interger. Time units (e.g. days) including the pollen emission 
#' (for simulation, default is time.max). Minimal value is a vector of 12 days.
#' @param density Plant density (number of plant by squared meter)
#' @param pollen Pollen production (number of grains by plant)
#' 
#' @return A matrix indexed by sources ID (in rows) and by time ( in columns) whose rows give 
#' the values of pollen emission (number of grains) for every source.
#' 
#' 
#' @export
#' 
create_pollen_sources <-
  function(sf,
           timeline = 1:61,
           density = runif(1, 7, 11),
           pollen = rgamma(1, shape = 1.6, scale = 1 / (2 * 10 ^ -7))) {
    
    ## add timeline 
    ls_timeline <- lapply(1:nrow(sf), function(i) return(timeline))
    sf[["timeline"]] <- ls_timeline

    # add pollen 
    ls_pollen_emission <- lapply(1:nrow(sf), function(i){
      density = density
      pollen = pollen
      nbr_days = length(ls_timeline[[i]])
      deb = sample(1:(nbr_days - length(maize.proportion_pollen)), 1)
      end = (deb + length(maize.proportion_pollen) - 1)
      pollen_emission <- rep(0, nbr_days)
      pollen_emission[deb:end] <- as.numeric(pollen * density * maize.proportion_pollen)
      return(pollen_emission)
    })
    sf[["pollen_emission"]] <- ls_pollen_emission

  return(sf)
}

#' @name create_pollen_sources
#' 
#' @export
#' 
create.pollen.sources <- function(){
  .Deprecated(create_pollen_sources, package="briskaR")
}