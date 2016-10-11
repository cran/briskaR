# demo.R 
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

#' @description Demonstration of briskaR package on Bt maize pollen (Genetically Modified crop) on non target Lepidoptera larvae.
#' @details In this example, 40\% of fields are sources (40 fields) and 100 individuals are exposed to toxic pollen. The time simulation of this demo is 30 days. 
#' 
#' This demo takes about 2 minutes running, according to the computer.
#' @title briskaR package pollen demonstration
#' @name demo.pollen.run
#' @param nb_fields number of fields (sources and neutrals) in the landscape (default 100)
#' @param max_size landscape size (in meter) (default 5000)
#' @param raster_size raster size (default 2^10)
#' @param nb_ind number of individuals to simulate (default 100)
#' @aliases demo.pollen.run briskaR-demo
#' @usage demo.pollen.run(nb_fields=100,max_size=5000,raster_size=2^10,nb_ind=100)
#' @keywords demo.pollen.run demo
#' @return A list of the simulated \link{Landscape-class} object, the \link{Individuals-class} object, and the \link{toxicIntensity} array [time, x, y].
#' The map of landscape with toxic intensity and individual states is plotted at time 30 (end of simulation).
#' @examples \dontrun{
#' # Demo for toxic pollen dispersion on (100) exposed individuals with 100 fields 
#' # in a squared domain of 5000x5000meters. 
#' # The domain is discretized by 1024x1024 pixels (2^10).
#' demo.pollen.run(nb_fields=100,max_size=5000,raster_size=2^10,nb_ind=100)
#' }
#' @include Class-Landscape.R Landscape-Methods.R Class-Individuals.R Individuals-Methods.R toxicIntensity.R ecoToxic.R pollen-data.R simulPrecip.R
#' @export
demo.pollen.run<-function(nb_fields=100,max_size=5000,raster_size=2^10,nb_ind=100) {
  
  # Number of day of simulation
  time.min = 1  #first day
  time.max = 30  #last day
  
  # simulate a landscape of size 5000x5000 meters with 100 fields and 0.4% of toxic fields
  l<-simulateInitialPartition(nb_fields,0.4,10,0,max_size,0,max_size)

  # simulate Margins fields on the prefious landscape
  l2<-simulateThickMargins(l)
  
  # individuals parameters
  dob=sample(time.min:time.max,size=nb_ind,replace=T)  # sample date fo birth for each indivuduals (12 is the pollen issued period)
  life_expectancies=rep(20,nb_ind)  # life duration for each individuals
  toxic_gap=rep(15,nb_ind)  # intern toxic threshold
  
  # create class individuals with parameters above
  ind<-simulateIndividuals(l2,n=nb_ind,mintime = time.min,maxtime = time.max, dob=dob,life_duration=life_expectancies,toxic_threshold=toxic_gap)
  plot(l2,ind)  # plot the landscape and individuals distribution
  
  # create toxic pollen issued period for each toxic fields
  list_sources<-getSPSources(l2) # get landscape sources fields (toxic)
  pollen.emis<-create.pollen.sources(0.4*nb_fields,time.max)  # 0.4% of 100 fields with time.max numbers of days
  pollen.emis<-data.frame(t=pollen.emis,row.names=row.names(list_sources))
  
  #precipitation on 60 days
  precip=c(0.0,0.0,0.0,0.0,6.5,1.5,1.5,1.5,0.0,0.0,0.0,0.5,4.0,0.0,0.0,4.0,5.0,3.0,1.5, 0.0,0.0,0.0,3.0,5.5,0.0,38.5,7.5,2.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.0,0.5,12.0,0.0,0.0,0.0,0.0,2.5,0.0,0.0,0.0,0.0,0.0,0.0,16.5,0.5,4.5,5.0,0.0,0.0,0.0,0.0,0.0,0.0,3.5)
  
  # Simulate toxic dispersion over the landscape
  td<-toxicIntensity(objectL=l2,toxic_emission=pollen.emis,mintime=time.min,maxtime=time.max,size_raster=raster_size,alpha=list(minalpha=0.1,maxalpha=0.95,covariate_threshold=30,simulate=F,covariate=precip))
  
  # Uncomment to active precipitation simulation
  # td<-toxicIntensity(objectL=l2,toxic_emission=pollen.emis,mintime=time.min,maxtime=time.max,size_raster=raster_size,alpha=list(minalpha=0.1,maxalpha=0.95,covariate_threshold=30,simulate=T,covariate=NULL))
  
  # Simulate toxic exposition on individuals
  ind2<-ecoToxic(l2,ind,td,time.min,time.max,kin=4.19e-7,kout=0.1,deltat=0.1)
  
  # plot toxic exposition and assimilation over time for the first individu
  plot(x=l2,y=ind2,objectT=td,numind=1)
  
  # plot individuals info at the end of the simulation
  plot(l2,ind2,time=time.max,objectT=td)
  
  return(list(landscape_demo=l2,individuals_demo=ind2,toxicIntensity_demo=td))
}
