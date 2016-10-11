# pollen-data.R 
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

data("maize.proportion_pollen",envir=environment())

#' @title Pollen sources emission simulation
#' @name create.pollen.sources
#' @description Simulate pollen sources emission for maize crop. The proportion of plants 
#' emitting pollen per day (during 12 days) was observed by Frédérique Angevin (Angevin et al. 2008). 
#' @param nbOfSource Number of source fields
#' @param numberOfDay Number of time units (e.g. days) including the pollen emission (for simulation, default is time.max). Minimal value is 12 days.
#' @param density Plant density (number of plant by squared meter)
#' @param pollen Pollen production (number of grains by plant)
#' @return A matrix indexed by sources ID (in rows) and by time ( in columns) whose rows give the values of pollen emission (number of grains) for every source.
#' @usage create.pollen.sources(nbOfSource=200, numberOfDay=60,
#' density=runif(1,7,11), pollen=rgamma(1,shape=1.6,scale=1/(2*10^-7)))
#' @examples 
#' # simulation of an emission matrix for 20 emitting sources and a emission period of 15 days:
#' create.pollen.sources(nbOfSource=20,numberOfDay=15)
#' # simulation of an emission matrix for 1 emitting source and a emission period of 20 days:
#' pollen.emission<-create.pollen.sources(nbOfSource=1,numberOfDay=20)
#' plot(pollen.emission[1,],xlab="time unites",ylab="maize crop emission")
#' @export
create.pollen.sources <- function(nbOfSource=200, numberOfDay=60, density=runif(1,7,11), pollen=rgamma(1,shape=1.6,scale=1/(2*10^-7))) {
  # courbe d'émission de pollen par jour à partir du début de floraison : données fournies par Frédérique Angevin Proportion of emitted pollen/silks, 12 jours d'émission de pollen
  #prop.pollen=c(0.0165,0.066,0.1545,0.1885,0.1735,0.156,0.1159,0.067,0.0377,0.0167,0.0055,0.0022)
  #prop.pollen = maize.proportion_pollen
#  data("maize.proportion_pollen",envir=environment())
  
  debut.emission=sample(1:(numberOfDay-length(maize.proportion_pollen)),nbOfSource,replace=T)
  pollen.emis=matrix(rep(0,numberOfDay*nbOfSource),ncol=numberOfDay,nrow=nbOfSource)
  
  for (i in 1:nbOfSource) {
    k=1                     
    debut=debut.emission[i]   
    for (t in debut:(debut+length(maize.proportion_pollen)-1)) {
      pollen.emis[i,t]=pollen*density*maize.proportion_pollen[k]  
      k=k+1
    }
  }
  
  return(pollen.emis)
}

# @export
#loadMaizeData <- function() {
#  data("maize_65")
#  data("maize.emitted_pollen")
#  data("maize.prop.pollen")
#  str(maize.individuals[1])
#  str(maize.individuals[1,10])
#}