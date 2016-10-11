# ecoToxic.R 
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

#' @importFrom graphics axis
#' @importFrom graphics abline

#' @title EcoToxicological model method
#' @description Generic method on Landscape and Individuals objects applying ecotoxicological equation.
#' @name ecoToxic
# @param objectL A Landscape object
# @param objectI An Individuals object
#' @param ... other parameters
#' @rdname ecoToxic
#' @exportMethod ecoToxic
setGeneric(name="ecoToxic",
           def=function(objectL,objectI,...)
             standardGeneric("ecoToxic")
)

# ecoToxic
#' @description This method gives internal concentration of contaminants within individuals, from toxic quantities in the environment and individual parameters.
# @details ecotoxic details
#' @param objectL A \link{Landscape} object
#' @param objectI An \link{Individuals} object
#' @param objectT A ToxicIntensityRaster object, a 3D array of Toxic Dispersion over time [t,x,y], first indice is time 
#' @param mintime Time to start simulation (default = 1)
#' @param maxtime Time to end simulation
#' @param kin ingestion rate (\% of contaminants staying in the body)
#' @param kout elimination rate (\% of contaminants eliminated from the body)
#' @param deltat \% of a time unit for the ordinary differential equation (ODE)
#' @return An \link{Individuals} object with updated internal toxic concentrations
#' @aliases ecotoxicological-method
#' @rdname ecoToxic
#' @examples \dontrun{
#' data(maize_65)
#' # Calculation of toxic internal concentration of individuals :
#' res <- ecoToxic(alandscape, anindividuals, atoxicintensity,
#'  mintime=1, maxtime=60, kin=0.25, kout=0.5, deltat=0.1)
#' # time series of toxic internal concentration for the 1st individual:
#' plot(alandscape, res, numind=1)
#' # Simulate pollen dispersion
#' tox <- toxicIntensity(maize.landscape,maize.emitted_pollen,1,61)
#' # Simulate ecotoxicological for individuals plot ecotoxicology of individual 1.
#' ind2<-ecoToxic(maize.landscape,maize.individuals,objectT=tox,maxtime=61)
#' # draw result for individual 1.
#' plot(maize.landscape,ind2,objectT=tox,numind=1)
#' }
#' 
setMethod(f="ecoToxic",
          signature=c("Landscape","Individuals"),
          definition=function(objectL,objectI,objectT,mintime=1,maxtime,kin=0.25,kout=0.5,deltat=0.1) {
            xstep=(objectL@xmax-objectL@xmin)/length(objectT[mintime,,1])
            ystep=(objectL@ymax-objectL@ymin)/length(objectT[mintime,1,])
            for(i in 1:objectI@n) {
              #cmilieu=array(NA,length(objectT[,1,1]))
              # raster start bottom left and matrix top left raster[x,y] ~ matrix[nrow-y,x]
              cmilieu<-objectT[,length(objectT[mintime,1,]) - round((objectI@coordinate[i]@coords[2]-objectI@ymin)/ystep),round((objectI@coordinate[i]@coords[1]-objectI@xmin)/xstep)]
              if(mintime > objectI@dob[i]) { cint<-objectI@intern_toxic[i,mintime-1] }
              else {cint<-0}
              conctemp<-conc.int(cmil=cmilieu,cint_start=cint,kin=kin,kout=kout,min.time=max(mintime,objectI@dob[i]),min(maxtime,objectI@dob[i]+objectI@life_duration[i]-1),deltat=deltat)
              objectI@intern_toxic[i,max(mintime,objectI@dob[i]):min(maxtime,(objectI@dob[i]+objectI@life_duration[i]-1))]<-conctemp
            }
            return(objectI)
        }
)




#' @title Plot internal toxic concentration method
#'
#' @description Plot a time series of internal toxic concentration for a given individual.
#' @name plotEcotoxic
#' @param objectL A \link{Landscape} object
#' @param objectI An \link{Individuals} object
#' @param objectT A ToxicIntensityRaster, a 3d array of Toxic intensity over the time [t,x,y], (first indice is time) see \code{\link{toxicIntensity}}
#' @param numind An individual ID
#' @examples \dontrun{
#' data(maize_65)
#' # Simulate pollen dispersion
#' tox <- toxicIntensity(maize.landscape,maize.emitted_pollen,1,61)
#' # plot ecotoxicology of individual 1.
#' ind2<-ecoToxic(maize.landscape,maize.individuals,objectT=tox,maxtime=61)
#' plot(maize.landscape,ind2,objectT=tox,numind=1)
#' }
#' @export
plotEcoToxic<-function(objectL,objectI,objectT,numind=1) {
  mintime=1
  xstep=abs((objectL@xmax-objectL@xmin)/length(objectT[mintime,,1]))
  ystep=abs((objectL@ymax-objectL@ymin)/length(objectT[mintime,1,]))
  par(mar=c(4.1,4.1,4.1,3.1))
  for(i in numind ) {
    plot(objectT[,length(objectT[mintime,,1]) - round((objectI@coordinate[i]@coords[2]-objectI@ymin)/ystep),round((objectI@coordinate[i]@coords[1]-objectI@xmin)/xstep)],col=1,xlab="time",ylab=" ",type="b",pch=16,las=1)
    max_local=max(objectT[,length(objectT[mintime,,1]) - round((objectI@coordinate[i]@coords[2]-objectI@ymin)/ystep),round((objectI@coordinate[i]@coords[1]-objectI@xmin)/xstep)])
    max_indiv=max(objectI@intern_toxic[i,])
    lines(objectI@intern_toxic[i,]*max_local/40,col="red",type="b",pch=16)
    points(objectI@dob[i],0,col=3,pch=16,cex=1.5)
    legend("topright",c("internal concentration","local concentration","date of birth"),col=c(2,1,3),pch=16 )
    title(main=paste("Individual #",numind))
    #axis(side=4,at=seq(0,max_local,max_local/4),labels=round(seq(0,max_indiv,max_indiv/4),2),las=1,col.axis=2)
    axis(side=4,at=seq(0,max_local,max_local/4),labels=round(seq(0,40,40/4),2),las=1,col.axis=2)
    mtext("local concentration",3,line=0,col=1,at=-1)
    mtext("internal concentration",3,line=0,col=2,at=60)
    abline(h=15*max_local/40,col="darkgrey")
    
    # plot the date of death, darkgrey -> toxic death , orange -> natural death
    indlife = getIndividualsLife(objectI)[i,]
    if ( min(indlife)==-2) {
      
      points(min(which(indlife == -2)),0,col="darkgrey",pch=21,lwd=3,cex=1.5) 
      abline(v=min(which(indlife == -2)),col="darkgrey")
    } else { 
      points(max(min(which(indlife == -1),max(which(indlife>0.0)))),0,col="orange",pch=21,cex=1.5,lwd=2)
    }
    
  }
}

###############
### PRIVATE ###
###############

# emily : modele ecotoxico simple : ashauer et al 2007
# cint : concentration interne à suivre dans le temps + seuil de mortalité à fixer sur cette concentration
# cmil : concentration dans le milieu (vecteur de 30 valeurs pour 30 jours)
# cint_start : concentration interne au debut
# kin : constante d'absorption ( 25% du pollen d'un pixel est absorbé par jour par larve)
# kout : constante d'élimination (50% du pollen est éliminé d'un jour au suivant)
# min.time temps de début
# max.time temps de fin
# deltat 
conc.int=function(cmil,cint_start=0,kin=0.25,kout=0.5,min.time=1,max.time,deltat=0.01) {
  if(min.time > max.time) {stop("ERROR : ecoToxic time value error")}
  cint=numeric(0)  
  cint_previous=cint_start
  for (t in min.time:max.time) {
    cmilieut<-cmil[t]
    cint_temp<-cint_previous
    j<-1
    indt<-deltat*kin*cmilieut
    outdt<-1 + deltat*kout
    for (dt in seq(0,0.99,deltat)) { # max.time+1
      # attention vecteur indices=entiers
      cint_temp[j+1]<-(cint_temp[j] + indt) / (outdt)
      j=j+1
    }
    cint[t]<-cint_temp[length(cint_temp)]
    cint_previous<-cint[t]
  }
  return(cint[min.time:max.time])
}
