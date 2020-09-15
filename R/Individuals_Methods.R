# Individuals-Methods.R
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


#' @title Wrapper function SimulateIndividuals
#' 
#' @name simulateIndividuals
#' 
#' @description This function simulates individuals as an Individuals object.
#'
#' Will simulate \code{n} individuals in receptors fields of a landscape.
#' 
#' @details The Individuals object output includes for each individual the
#' coordinates, the date of birth, the life duration, the toxic threshold and
#' 
#' 
#' @param sf A \link[sf]{sf} object
#' @param size Number of individuals to simulate
#' @param timeline Vector of the time line
#' @param dob A vector for the Date Of Birth of each individual between
#' \code{min} and \code{max} of the \code{timeline}.
#' @param life_duration A vector for the life duration of each individual
#' @param toxic_threshold A vector for the internal toxic threshold value leading to death for each individual
#' 
#' @export
#' 
simulateIndividuals <- function(sf,
                               size = 100,
                               timeline = 1:61,
                               dob,
                               life_duration,
                               toxic_threshold){
  
  sample_points <- st_sf( geometry = st_sample(sf, size = size, type = "random", exact = TRUE))
  
  sample_points$dob <- sample(dob, size = nrow(sample_points), replace = TRUE) 
  sample_points$life_duration <- sample(life_duration, size = nrow(sample_points), replace = TRUE) 
  sample_points$toxic_threshold = sample(toxic_threshold, size = nrow(sample_points), replace = TRUE) 
  
  iter_vec <- 1:nrow(sample_points)
  
  sample_points$timeline <- lapply(iter_vec, function(i) return(timeline))
  sample_points$life_boolean <- lapply(iter_vec, function(i) return(timeline*0))
  sample_points$life_boolean <-  lapply(iter_vec, function(i){
      vec = sample_points$life_boolean[[i]]
      deb = min(sample_points$dob[i], max(sample_points$timeline[[i]]))
      end = min(sample_points$dob[i]+sample_points$life_duration[i], max(sample_points$timeline[[i]]))
      vec[deb:end] = 1
      return(vec)
  })
  
  return(sample_points)
}

#' @title Wrapper function loadIndividuals
#' @name loadIndividuals
#' @description Wrapper function to create an individuals object using \link[sf]{sf} or \link[sp]{SpatialPoints} and data.frame.
#'
#' The SpatialPoints object and the data.frame have to contain the same number of coordinates and rows.
#' 
#' @param sf a \link[sf]{sf} object
#' @param data a data.frame containing individuals attributes.
#' Rows numbers as individuals ID, columns names as dob (date of birth) | life_duration | toxic_threshold
#' @param timeline Vector of the time line
#' 
#' @return an \code{Individuals} object
#' 
#' @export
#' 
loadIndividuals <- function(sf, data, timeline) {

  sample_points <- sf
  
  sample_points$dob <- data$dob
  sample_points$life_duration <- data$life_duration
  sample_points$toxic_threshold = data$toxic_threshold

  sample_points <- brk_addFD(sample_points, key = "timeline", FUN = function(i) return(timeline))
  
  return(sample_points)
}