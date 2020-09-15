# Individuals-Methods.R
# Part of the briskaR package.
#
# Copyright (C) 2019        virgile Baudrot <virgile.baudrot@posteo.net>
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

#' @name brk_emission 
#' 
#' @title Wrapper function brk_emission
#' 
#' 
#' @description 
#' 
#' `brk_emission` This function simulates emissions. Will simulate emissions shape in source fields of a landscape.
#' 
#' @details This function is a wrapper of with and lapply function and is like this: 
#'                sf[["key"]] <- with(sf, lapply(1:nrow(sf), FUN, ...))
#'  
#'  So, all column of sf can be called in FUN
#' 
#' @param sf A \link[sf]{sf} object
#' @param keyTime character. Name of the column for timeline. Used to check length of vectors in column vector object
#' @param key character. Name of the column which is going to be created 
#' @param FUN A function applied on the sf object. See lapply functions for further details assuming \code{X = 1:nrow(sf)}, that is \code{sf[[key]] <- lapply(1:nrow(sf), FUN, ...)}.
#' 
#' @export
#' 
brk_emission <- function(sf, # geometry on which we applied the pollen emission pattern
                        keyTime,
                        key, # name of the column which is going to be created
                        FUN # a function of timeline?
){
  if("stackTimeline" %in% colnames(sf)) {
    stop("Please rename column 'stackTimeline' in sf object.")
  }
  # CREATE NEW COLUMN
  sf[["key_temp"]] <- lapply(1:nrow(sf), FUN)
  # check length with timeline
  if(all(sapply(sf[[keyTime]], length) != sapply(sf[["key_temp"]], length))){
    stop(paste0("element within list returned by FUN' has not the same length as list element of '", keyTime, "' column"))
  }
  # ADD StackTimeLine
  stackTimeline = sort(unique(do.call("c", sf[[keyTime]])))
  
  sf[[key]] = lapply(1:nrow(sf), function(i){
    index_matching = match(sf[[keyTime]][[i]], stackTimeline)
    res = rep(0,length(stackTimeline))
    res[index_matching] = sf[["key_temp"]][[i]]
    return(res)
  })
  # /!\ REPLACE timeline
  sf[[keyTime]] = lapply(1:nrow(sf), function(i) stackTimeline)
  warning(paste("The column variable", keyTime, "may have changed"))
  # remove "key_temp"
  sf[["key_temp"]] = NULL
  
  return(sf)
}


# brk_emission_ <- function(sf, # geometry on which we applied the pollen emission pattern
#                          keyTime,
#                          key, # name of the column which is going to be created
#                          FUN){
#   if("stackTimeline" %in% colnames(sf)) {
#     stop("Please rename column 'stackTimeline' in sf object.")
#   }
#   # CREATE NEW COLUMN
#   #
#   #/!\ CRAPPY FUNCTION FOR EVALUATION OF FUN BASED ON OBJECT OF SF !!!
#   sf[["key_temp"]] <- withLapply(sf, FUN)
#   # ------------------------------------------ SHITTY
#   # check length with timeline
#   if(all(sapply(sf[[keyTime]], length) != sapply(sf[["key_temp"]], length))){
#     stop(paste0("element within list returned by FUN' has not the same length as list element of '", keyTime, "' column"))
#   }
#   # ADD StackTimeLine
#   stackTimeline = sort(unique(do.call("c", sf[[keyTime]])))
#   
#   sf[[key]] = lapply(1:nrow(sf), function(i){
#     index_matching = match(sf[[keyTime]][[i]], stackTimeline)
#     res = rep(0,length(stackTimeline))
#     res[index_matching] = sf[["key_temp"]][[i]]
#     return(res)
#   })
#   # /!\ REPLACE timeline
#   sf[[keyTime]] = lapply(1:nrow(sf), function(i) stackTimeline)
#   warning(paste("The column variable", keyTime, "may have changed"))
#   # remove "key_temp"
#   sf[["key_temp"]] = NULL
#   
#   return(sf)
# }

# --- LOCAL FUNCTION for brk_emission !!
# 
# withLapply = function(x, FUN){
#   with(x,
#        lapply(1:nrow(x), {environment(FUN) <- environment(); FUN}))
# }
# 
# withLapply2 = function(x, FUN, ...){
#   attach(x)
#   toReturn = lapply(1:nrow(x), FUN, ...)
#   on.exit(expr = detach(x), add=TRUE)
#   return(toReturn)
# }

#' @name brk_emission 
#' 
#' @title Compute emission function
#' 
#' @description `brk_emission_landscape` Simulate sources emission intensity. With:
#'  - `emission` is the quantity of emission per time unit per spatial unit. With other argument, we have:
#'  - `emission = density x intensity = density x production x intensity_pmf`.
#'  - `density` is the density of the source, so the quantity of source per spatial unit.
#'  - `intensity` is the intensity of the emission for 1 source: that is the quantity of emission per time unit per source.
#'  - `production` is the overall production, the total emission, for one source unit: quantity of emission for the period
#'  - `intensity_pmf` is the distribution of emission along time. So we have `intensity = production x intensity_pmf`.
#'  
#' 
#' @param timeline Vector of time units (e.g. days) covering all the function
#' @param emission Vector or Matrix given the quantity of emission per time unit per spatial unit.
#' Length of vector equal the length of the `timeline` vector (time unit matching).
#' Size of the matrix, `n,m`, is such as the number of row equal the number of sources in `sf` object, and the number of column
#' equals the length of the `timeline` vector.
#' @param density Scalar or Vector (with length equal to the number of sources in `sf` object) given the density
#'  of the source(s) (e.g. number of plant by squared meter)
#' @param intensity Vector or Matrix given the quantity of emission per time unit per source.
#' Length of vector equal the length of the `timeline` vector (time unit matching).
#' Size of the matrix, `n,m`, is such as the number of row equal the number of sources in `sf` object, and the number of column
#' equals the length of the `timeline`.
#' @param intensity_pmf Vector or Matrix given distribution of emission along time (given a probability mass function with time) .
#' Length of vector equal the length of the `timeline` vector (time unit matching).
#' Size of the matrix, `n,m`, is such as the number of row equal the number of sources in `sf` object, and the number of column
#' equals the length of the `timeline`.
#' @param production Scalar or Vector (with length equal to the number of sources in `sf` object) total emission for one source (e.g. total number of grains by plant)
#' 
#' @return A matrix indexed by sources ID (in rows) and by time ( in columns) whose rows give 
#' the values of intensity emission (number of grains) for every source.
#' 
#' 
#' @export
#' 
brk_emission_landscape <-
  function(sf,
           timeline = 1:61,
           emission = NULL,
           density = NULL,
           intensity = NULL,
           intensity_pmf = NULL,
           production = NULL) {
    
    if(!is.null(emission)){ # keep emission
    } else if (!is.null(intensity)){
      emission = density * intensity
    } else if(!is.null(intensity_pmf & production)){
      emission = density * intensity_pmf * production
    }
    
    ## add timeline
    ls_timeline <- lapply(1:nrow(sf), function(i) return(timeline))
    sf[["timeline"]] <- ls_timeline
    
    # add intensity 
    ls_emission <- lapply(1:nrow(sf), function(i){
      if(is.vector(emission)){emission}
      if(is.matrix(emission)){emission[i,]}
    })
    sf[["emission"]] <- emission
    return(sf)
  }