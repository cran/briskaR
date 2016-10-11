# Landscape-Individuals-Methods.R 
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

#' @title plot Landscape & Individuals method
#' 
#' @description Plots using Landscape & Individuals objects.
#' @details If objectT and numind are informed, the function will draw an individual ecoToxical plot.
#' 
#'  If objectT and time are specified, the function will draw the landscape, the individuals states and the toxic intensity at this time of the simulation.
#'  
#'  By default this function will draw the landscape and the individuals positions.
#' 
#' @param x A Landscape object
#' @param y An Individuals object
#' @param time Time selection (default = -1) for Toxic intensity
#' @param add Boolean to draw hover an another plot
#' @param objectT ToxicIntensityRaster, 3D array (result of \link{toxicIntensity}, default NULL)
#' @param numind an individual ID
#' @param ... default plot parameters (par)
#' @param plot.legend plot legend (default TRUE)
#' @rdname Landscape-Individuals-plot-method
#' @aliases plot,Landscape,Individuals-method
#' @examples \dontrun{
#' data(maize_65)
#' # plot a landscape and individuals
#' plot(maize.landscape,maize.individuals)
#' # plot a landscape and individuals states at time 30
#' plot(maize.landscape,maize.individuals,time=30)
#' # Simulate pollen dispersion
#' tox <- toxicIntensity(maize.landscape,maize.emitted_pollen,1,61)
#' # plot a landscape, individuals and pollen dispersion of maize_65 data
#' plot(maize.landscape,maize.individuals,time=30,objectT=tox)
#' # plot ecotoxicology of individual 1.
#' ind2<-ecoToxic(maize.landscape,maize.individuals,objectT=tox,maxtime=61)
#' plot(maize.landscape,ind2,objectT=tox,numind=1)
#' }
#' @include Class-Landscape.R Landscape-Methods.R Class-Individuals.R Individuals-Methods.R toxicIntensity.R
#' @export
setMethod(f="plot",
          signature=c("Landscape","Individuals"),
          definition=function(x,y,time=-1,objectT=NULL,numind=-1,add=F,...,plot.legend=TRUE) {
            
            par(xpd=T)
            par(pty="m")
            
            if(time == -1) {
              if(numind != -1 & !is.null(objectT) & class(objectT) == "ToxicIntensityRaster"){
                plotEcoToxic(x,y,objectT,numind)
              }
              else {
                plot(x,add=add,..., plot.legend=plot.legend)
                plot(y,add=TRUE,..., plot.legend=plot.legend)  
              }
            }
            else {
              #if( is.null(nrow(time)) & !is.null(objectT) & class(objectT) == "ToxicIntensityRaster") {
                plot(x,add=add,objectT=objectT,time=time,...,plot.legend=plot.legend,plot.legend.raster=FALSE)
                plot(y,time,add=T,...,plot.legend=plot.legend)
                if(plot.legend == TRUE & is.null(objectT) & class(objectT) == "ToxicIntensityRaster") {
                  p<-heat.colors(100, alpha = 0.6)
                  p[100]=rgb(0,0,0,alpha=0)
                  mtext(text =paste("Time",time,sep=" "),at=par("usr")[1]+0.45*diff(par("usr")[1:2]),line = -1,side = 3)
                  fields::image.plot(as.matrix(objectT[time,,]),legend.only=T,smallplot=c(0.85,0.88,0.20,0.90),col=p[length(p):1],add=T,graphics.reset=FALSE)
                  mtext(text ="Toxic intensity",line = 0,side = 3,adj = 1.1,padj = 1)
                  }
              #}
            }
            
#             if(time != -1 & !is.null(objectT) & numind == -1) {
#                 #p<-rainbow(100,alpha=0.3,start=0,end=0.7)
#                 p<-heat.colors(100, alpha = 0.6)
#                 p[100]=rgb(0,0,0,alpha=0)
#                 temp<-objectT[time,,]
#                 temp[which(temp<=0)] <- NA
#                 r<-raster(as.matrix(temp),crs=CRS(BRISKAR_INTERN_PROJECTION))
#                 extent(r)<-extent(x@xmin,x@xmax,x@ymin,x@ymax)
#                 if( !is.na(proj4string(x@thelandscape)) ) { r<-projectRaster(r,crs=proj4string(x@thelandscape)) }
#                 raster::image(r,col=p[length(p):1],useRaster=F,add=T,bg="transparent")
#                 # fast way but not in coordinates mapping
#                 #image(x=seq(x@thelandscape@bbox[1,1],x@thelandscape@bbox[1,2],(x@thelandscape@bbox[1,2]-x@thelandscape@bbox[1,1])/nrow(temp)),y=seq(x@thelandscape@bbox[2,1],x@thelandscape@bbox[2,2],(x@thelandscape@bbox[2,2]-x@thelandscape@bbox[2,1])/ncol(temp)),z=t(as.matrix(temp))[1:nrow(temp),ncol(temp):1],col=p[length(p):1],useRaster=T,add=T,bg="transparent")
#                 if(plot.legend == TRUE) {
#                   fields::image.plot(as.matrix(temp),legend.only=T,smallplot=c(0.85,0.88,0.20,0.90),col=p[length(p):1])
#                   mtext(text ="Toxic intensity",line = 0,side = 3,adj = 1.1,padj = 1)
#                 }
#             }
            
          }
)
