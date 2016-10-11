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
#' @name simulateIndividuals
#' @description This function simulates individuals as an Individuals object.
#' 
#' Will simulate \code{n} individuals in receptors fields of a \link{Landscape}.
#' @details The Individuals object output includes for each individual the coordinates, the date of birth, the life duration, the toxic threshold and the internal toxic concentration.
#' @rdname simulateIndividuals-constructor-class
#' @param objectL A \link{Landscape} object
#' @param n Number of individuals to simulate
#' @param mintime Start simulation time
#' @param maxtime End simulation time
#' @param dob A vector for the Date Of Birth of each individual between \code{mintime} and \code{maxtime}
#' @param life_duration A vector for the life duration of each individual
#' @param toxic_threshold A vector for the internal toxic threshold value leading to death for each individual 
#' @return A S4 \link{Individuals-class} object
#' @examples 
#' data(maize_65)
#' nb_ind=100 ; time_min=1 ; time_max=61
#' birth_dates=sample(time_min:time_max, size=nb_ind, replace=TRUE)
#' life_expectancies=rep(20, nb_ind)
#' toxic_gap=rep(15, nb_ind)
#' ## generate exposed individuals
#' ind=simulateIndividuals(maize.landscape, n=nb_ind, mintime=time_min,
#'                         maxtime=time_max, dob=birth_dates,
#'                         life_duration=life_expectancies, toxic_threshold=toxic_gap)
#' plot(maize.landscape,ind)
#' @seealso \link{loadIndividuals}
#' @include Class-Landscape.R Class-Individuals.R
#' @export
simulateIndividuals <- function(objectL,n=200,mintime=1,maxtime,dob,life_duration,toxic_threshold) {
  
  res <- new('Individuals')
  res@n = n
  res@mintime=mintime
  res@maxtime=maxtime
  
  # Poisson (aléatoire)
  res@coordinate<-spsample(getSPReceptors(objectL),n,"random",iter=4,bb=objectL@thelandscape@bbox)
  res@coordinate@bbox<-objectL@thelandscape@bbox
  res@xmin<-objectL@xmin
  res@xmax<-objectL@xmax
  res@ymin<-objectL@ymin
  res@ymax<-objectL@ymax
  res@dob=dob
  #res@adult=res@dob
  res@life_duration=life_duration
  res@intern_toxic=matrix(data=0,nrow=n,ncol=maxtime)
  res@toxic_threshold=toxic_threshold
  return (res)
}

#' @title Plot method for Individuals-class
#' @name Individuals plot 
#' @description Will plot \link{Individuals} spatial positions.
#' @param x An Individuals object
# @param y missing (not use)
#' @param add if True the new plot will overlap an already plot image (default False) 
#' @param ... further graphical parameters (\code{par})
#' @param plot.legend plot legend (default TRUE)
#' @rdname Individuals-plot-methods
#' @aliases plot,Individuals,ANY-method
#' @examples
#' data(maize_65)
#' plot(maize.individuals)
#' @export
setMethod(f="plot",
          signature=c("Individuals",NULL),
          definition=function(x,y,add=F,...,plot.legend=TRUE) {
            if(add==F){
              plot(c(x@xmin,x@xmax),c(x@ymin,x@ymax),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
              rect(x@xmin,x@ymax,x@xmax,x@ymin,lwd=1) 
            }
            plot(x@coordinate,pch=4,lwd=3,col="blue",add=T,...)
            if(plot.legend) {
              legend(x=x@coordinate@bbox[1,1]+(x@coordinate@bbox[1,2]-x@coordinate@bbox[1,1])*0.3,y=x@coordinate@bbox[2,1],c("individuals"),pch=4,pt.lwd=3,col=c("blue"))
            }
          }
)

#' @title Plot method for Individuals
#' @description Will plot individuals positions and state at a time of the simulation.
#' @details The states correspond to internal concentration of toxic.
#' "Red" cross means that the individual is dead because of toxic exposition.
#' "Green" cross means that the individual is dead by natural death.
#' "Green" to "Red" points give the gradient of toxic concentration before the threshold.
# @param x An Individuals object
#' @param y time of the simulation to display individuals
# @param add if True this plot will plot overlap an already plot image (default False) 
# @param ... further graphical parameters (\code{par})
#' @examples 
#' data(maize_65)
#' # individuals locations
#' plot(maize.individuals)
#' # individuals at time 61
#' plot(maize.individuals,61)
#' #individuals at time 61 with the landscape
#' plot(maize.landscape,maize.individuals,time=61)
#' @rdname Individuals-plot-methods
#' @aliases plot,Individuals,num-method
#' @export
setMethod(f="plot",
          signature=c("Individuals","numeric"),
          definition=function(x,y,add=F,...,plot.legend=TRUE) {
            if(add==F) {
              plot(c(x@xmin,x@xmax),c(x@ymin,x@ymax),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
              rect(x@xmin,x@ymax,x@xmax,x@ymin,lwd=1) 
            }
            
            p<-colorRampPalette(c("green","red"))
            rainbow<-p(10)
            #rect(min(x@coordinate$x)-50,min(x@coordinate$y)-50,max(x@coordinate$x)+50,max(x@coordinate$y)+50,col="white")
#             if( length(ind<-which(x@dob<=y & x@adult>y & x@intern_toxic[,y]<x@toxic_threshold)) != 0) {
#               plot(x@coordinate[ind],pch=1,col="blue",add=T,...)
#             }
#             if( length(ind<-which(x@dob<=y & x@intern_toxic[,y]<x@toxic_threshold & x@dob+x@life_duration>y)) != 0) {
#               #plot(x@coordinate[ind],pch=16,col="green",add=T,...)
#               for(i in ind) {
#                 points(x@coordinate[i],pch=16,col=rainbow[round((x@intern_toxic[i,y]*length(rainbow))/x@toxic_threshold[ind])+1])
#               }
#             }
#             if( length(ind<-which(x@dob<=y & x@intern_toxic[,y]>=x@toxic_threshold & x@dob+x@life_duration>y)) != 0) {
#               plot(x@coordinate[ind],pch=16,col="red",add=T,...)
#             }
#             if( length(ind<-which(x@dob+x@life_duration<=y & max(x@intern_toxic[,x@mintime:x@maxtime])>=x@toxic_threshold)) != 0) {
#               plot(x@coordinate[ind],pch=4,col="red",add=T,...)
#             }
#             # may show only natural dead
#             if( length(ind<-which(x@dob+x@life_duration<=y & max(x@intern_toxic[,x@mintime:x@maxtime])<x@toxic_threshold)) != 0) {
#               plot(x@coordinate[ind],pch=3,col="white",add=T,...)
#             }
            lt<-getIndividualsLife(x)
            
            if( length(ind<-which(lt[,y] == -1)) != 0 ) {
              plot(x@coordinate[ind],pch=3,cex=1.5,lwd=3,col=rgb(0,180,0,maxColorValue = 255),add=TRUE,...)
            }
            
            if( length(ind<-which(lt[,y] == -2)) != 0 ) {
              plot(x@coordinate[ind],pch=4,cex=1.5,lwd=3,col="red",add=TRUE,...)
            }
            
            if( length(ind<-which(lt[,y] > 0)) != 0 ) {
              for(i in ind) {
                plot(x@coordinate[i],pch=16,cex=1.5,lwd=3,col=rainbow[round((x@intern_toxic[i,y]*(length(rainbow)+1))/x@toxic_threshold[i])+1],add=TRUE,...)
              }
            }
            
            if(plot.legend) {
              legend(x=x@coordinate@bbox[1,1]+(x@coordinate@bbox[1,2]-x@coordinate@bbox[1,1])*0.3,y=x@coordinate@bbox[2,1],c("natural death","toxic death","alive"),pch=c(3,4,16),pt.cex=1.5,pt.lwd=3,col=c(rgb(0,180,0,maxColorValue = 255),"red","green"),title="Individuals",bg=rgb(255,255,255,alpha=100,maxColorValue=255))
              legend(x=x@coordinate@bbox[1,1]+(x@coordinate@bbox[1,2]-x@coordinate@bbox[1,1])*0.6,y=x@coordinate@bbox[2,1],c("low","medium","threshold"),pch=c(16,16,16),pt.cex=1.5,pt.lwd=3,col=c(rainbow[1],rainbow[5],rainbow[10]),title="Internal concentration",bg=rgb(255,255,255,alpha=100,maxColorValue=255))
            }
          }
)


#' @title Show a summary of Individuals information
#' @description print a summary of Individuals information (the 10 first individuals)
# @name show Individuals
#' @param object An \link{Individuals} object
#' @rdname Individuals-show-method
#' @examples
#' data(maize_65)
#' maize.individuals
#' @aliases show,Individuals-method
setMethod(f="show",
          signature="Individuals",
          definition=function(object) {
            cat("*** Class Individuals, method Show ***\n")
            cat(sprintf("* Numbers = %i\n",object@n))
            
            nbIndMin <- min(10,length(object@dob))
            if(nbIndMin!=0) {
              cat(sprintf("* %s first Individuals\n",nbIndMin))
              cat("Coordinate :\n")
              for(i in 1:nbIndMin) { print(object@coordinate[i])}
              cat("Date of Birth :\n")
              print(object@dob[1:nbIndMin])
              
              cat("Intern concentrations (date of birth + 10) :\n")
              for(i in 1:nbIndMin) {
                indtimemax <- min(object@dob[i]+10,object@maxtime)
                cat(i," : ", object@intern_toxic[i,object@dob[i]:indtimemax],"\n")
              }
              #print(object@intern_toxic[1:nbIndMin,object@dob[1:nbIndMin]:(object@dob[1:nbIndMin]+10)])
            }
            cat("*** End Show(Individuals) ***\n")  
          }          
)

# @title Print part of Individuals info
# @name print
#' print Individuals informations
#' @param x An \link{Individuals} object
#' @param ... further arguments passed to or from other methods.
#' @rdname Individuals-print-class
#' @examples
#' data(maize_65)
#' print(maize.individuals)
#' @aliases print,Individuals-method 
setMethod(f="print",
          signature="Individuals",
          function(x,...) {
            cat("*** Class Individuals, method print ***\n")
            cat(sprintf("* Numbers = %i\n",x@n))
            
            nbIndMax <- max(10,length(x@dob))
            if(nbIndMax!=0) {
              cat(sprintf("* %s Individuals\n",nbIndMax))
              cat("Coordinate :\n")
              for(i in 1:nbIndMax) { print(x@coordinate[i])}
              cat("Date of Birth :\n")
              print(x@dob[1:nbIndMax])
              
              cat("Intern concentrations (date of birth + 10) :\n")
              print(x@intern_toxic[1:nbIndMax,])
            }
            cat("*** End print(Individuals) ***\n")  
          }
)

# getIndividuals[i] retourne info individue i 
#' @name [,Individuals
 #,numeric,ANY,ANY
#' @title Get an individual information
#' @description Get an Individual information
#' 
#' x[i] get individual i values from \link{Individuals} x.
#' @param x An \link{Individuals} object
#' @param i individual index
# @param ... further arguments passed to or from others methods.
# @param drop logical value (default = TRUE)
#' @return a data.frame with all Individuals slot(attributes) for an individual
#' @rdname Individuals-get-methods
#' @aliases [,Individuals,numeric,missing,ANY-method
#' @examples 
#' data(maize_65)
#' # get individual 99 informations
#' str(maize.individuals[99])
# @usage
# # S4 method to access Individuals values for signature 'Individuals,numeric'
# x[i]
#' @export
setMethod(
  f="[",
  signature=c(x="Individuals",i="numeric",j="missing"),#,drop="missing"),
  definition=function(x,i) {
    if(i<=x@n) {
      df<-data.frame(id=c(i),coodinate=c(x@coordinate[i]),dob=c(x@dob[i]),life_expectencies=c(x@life_duration[i]),toxic_threshold=c(x@toxic_threshold[i]))
      res<-cbind(df,"intern_toxic"=matrix(x@intern_toxic[i,],ncol=length(x@intern_toxic[i,])))
      return(res)
      #return(list("coodinate"=x@coordinate[i],"dob"=x@dob[i],"concentration"=x@intern_toxic[i,]))
    }
    else {return(data.frame())}
  }
)

# getIndividuals[i] retourne info individue i 
# [i,t] retourne info a temps t
# @name [,Individuals
 #,numeric,numeric,ANY
#' @title Get an individual information at a specified time
#' @description Get an Individual information at a specified time
#' 
#' x[i,j] get individual i at time j values from \link{Individuals} x.
# @param x An \link{Individuals} object
# @param i individual index
#' @param j time of information
# @param ... further arguments passed to or from others methods.
# @param drop logical value (default = TRUE)
#' @return a data.frame with all Individuals slot (attributes) for an individual at a specified time.
#' @rdname Individuals-get-methods
#' @aliases [,Individuals,numeric,numeric,ANY-method
#' @examples 
#' data(maize_65)
#' #get individual 99 informations at time 30
#' maize.individuals[99,30]
# @usage
# # S4 method to access Individuals values for signature 'Individuals,numeric,numeric'
# x[i,j]
#' @export
setMethod(
  f="[",
  signature=c(x="Individuals",i="numeric",j="numeric"),#,drop="missing"),
  definition=function(x,i,j) {
     if(i<=x@n && !missing(j) && j<=x@maxtime) {
       df<-data.frame(id=c(i),coodinate=c(x@coordinate[i]),dob=c(x@dob[i]),life_expectencies=c(x@life_duration[i]),toxic_threshold=c(x@toxic_threshold[i]),intern_toxic=c(x@intern_toxic[i,j]))
       return(df)
       #return(list("coodinate"=x@coordinate[i],"dob"=x@dob[i],"concentration"=x@intern_toxic[i,j]))
     }
     else {return(data.frame())}
  }
)

#' @title Method to get Individuals Life information
#' @name getIndividualsLife
# @param object An Inidividuals object
#' @param ... other parameters
#' @rdname Individuals-getIndividualsLife-method
#' @exportMethod getIndividualsLife
setGeneric(name="getIndividualsLife",
           def=function(object,...)
             standardGeneric("getIndividualsLife")
)

#' getIndividualsLife
#'
#' @description Get individuals toxic concentration over the simulation time and return life states for individuals.
#' @details If intern concentration overtakes the toxic threshold value is "-2", that means the individual is dead because of higher toxic concentration.
#' Otherwise value is "-1" means the individual is dead in natural way. The value "0" means that the individual is not alive yet.
#' @param object An Individuals object
#' @return a matrix indexed by individual ID in rows and by time in columns.
#' @aliases getIndividualsLife,Individuals-method
#' @rdname Individuals-getIndividualsLife-method
#' @examples 
#' # the fifth first individuals states for each time step
#' data(maize_65)
#' life<-getIndividualsLife(maize.individuals)
#' matplot(1:10,life[1:10,],type='l',col=1:5,xlab="source",ylab="states")
setMethod(f="getIndividualsLife",
          signature="Individuals",
          definition=function(object) {
            res<-object@intern_toxic
            for(ind in 1:object@n)
            {
              for(t in object@dob[ind]:min(object@maxtime,(object@dob[ind]+object@life_duration[ind])))
              {
                if(res[ind,t] >= object@toxic_threshold[ind]) { res[ind,t:object@maxtime]<- -2;break}
              }
              if( (object@dob[ind]+object@life_duration[ind]) <= object@maxtime) {if(res[ind,object@dob[ind]+object@life_duration[ind]] != -2) {res[ind,(object@dob[ind]+object@life_duration[ind]):object@maxtime]<- -1}}
            }
            return(res)
          }
)

#' @title Wrapper function loadIndividuals
#' @name loadIndividuals
#' @description Wrapper function to create an \link{Individuals} object using SpatialPoints and data.frame .
#' 
#' The SpatialPoints object and the data.frame have to contain the same number of coordinates and rows.
#' @rdname load-Individuals-class
#' @param objectL a \link{Landscape} object
#' @param sp a SpatialPoints object (individuals coordinates)
#' @param data a data.frame containing individuals attributes. Rows numbers as individuals ID, columns names as dob (date of birth) | life_duration | toxic_threshold
#' @param mintime Start simulation time
#' @param maxtime End simulation time
#' @examples  
#' library(sp)
#' data(maize_65)
#' # simulate 2 individuals coordinates (SpatialPoints object) and data:
#' coordinates <- SpatialPoints(matrix(c(468232.1,6259993,464848.8,6260483),ncol=2,byrow=TRUE),
#' proj4string=CRS("+init=epsg:2154"))
#' df <- data.frame("dob"=c(1,8),"life_duration"=c(20,20),
#'            "toxic_threshold"=c(15,15),row.names = c(1,2))
#' # create an Indiviudals object from previous data
#' ind <- loadIndividuals(objectL=maize.landscape,sp=coordinates,data=df,mintime=1,maxtime=61)
#' plot(maize.landscape,ind)
#' @return an \code{\link{Individuals}} object
#' @export
loadIndividuals<-function(objectL,sp,data,mintime,maxtime) {
  
  res <- new("Individuals")
  
  res@n=length(sp)
  res@coordinate=sp
  res@coordinate@bbox<-objectL@thelandscape@bbox
  res@xmin=objectL@xmin
  res@xmax=objectL@xmax
  res@ymin=objectL@ymin
  res@ymax=objectL@ymax
  res@dob=data$dob
  res@life_duration=data$life_duration
  #res@adult=data$adult
  res@intern_toxic=matrix(data=0,nrow=res@n,ncol=maxtime)
  res@toxic_threshold=data$toxic_threshold
  res@mintime=mintime
  res@maxtime=maxtime
  
  return(res)
}