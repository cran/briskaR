# Landscape-Methods.R
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


#' @title simulateInitialPartition Method
#' 
#' @description This function creates an object \link[sp]{SpatialPolygonsDataFrame} and simulates a landscape with neutral and source fields.
#' 
#' @name simulateInitialPartition
#' 
#' @importFrom MASS mvrnorm
#' @import sp
#' @importFrom sf st_as_sf
#' 
#' @param n Numeric, numbers of cells
#' @param prop Numeric [0,1] toxic cells proportion
#' @param range Aggregation parameter (range of the spatial Exponential covariance of Gaussian process)
#' @param xmin x-axis left coordinates in space unit (see projections_briskaR)
#' @param xmax x-axis right coordinates in space unit (see projections_briskaR)
#' @param ymin y-axis bottom coordinates in space unit (see projections_briskaR)
#' @param ymax y-axis top coordinates in space unit (see projections_briskaR)
#' 
#' @return An \link[sp]{SpatialPolygonsDataFrame} object with \code{n} fields, \code{prop} pourcentage of toxic 
#' fields of size (xmin,xmax) (ymin,ymax)
#' 
#' @details In the function the first step is a binomial point process to simulate a \link[sp]{SpatialPointsDataFrame}
#' with sources and neutral marks, which depends on the aggregation parameter.
#' The second step of the function is the Voronoi tesselation from the simulated points and 
#' returns a \link[sp]{SpatialPolygonsDataFrame}.
#' 
#' @examples \dontrun{
#' # Simulate a 5000m x 5000m landscape with 500 cells (e.g. fields)
#' # whose 40% (200 cells) are sources.
#' # The projection by default is Lambert93 projection.
#' land <- simulateInitialPartition(n=500,prop=0.4,range=10,xmin=0,xmax=5000,ymin=0,ymax=5000)
#' plot(land)
#' }
#' 
#' @export
#' 
simulateInitialPartition <-
  function(n = 500,
           prop = 0.4,
           range = 10,
           xmin = 0,
           xmax = 5000,
           ymin = 0,
           ymax = 5000) {
    points <- create.voronoi.points(n, prop, range, xmin, xmax, ymin, ymax)
    map <- create.voronoi.diagram(points, xmin, xmax, ymin, ymax)
    map <- sf::st_as_sf(map)
    return(map)
  }


#' @title Simulate a new landscape
#' 
#' @name simulateLandscape
#' 
#' @description Create an object of class \link[sp]{SpatialPolygonsDataFrame}. Simulate a landscape with neutral and source fields and receptors margins.
#' 
#' @details Execute both \link{simulateInitialPartition} and \link{simulateThickMargins} functions.
#' 
#' 
#' @param n Numeric, numbers of fields
#' @param prop Numeric [0,1] toxic fields proportion
#' @param range aggregation parameter (range of the spatial exponential covariance of gaussian process) in meters.
#' @param xmin x-axis left coordinates
#' @param xmax x-axis right coordinates
#' @param ymin y-axis bottom coordinates
#' @param ymax y-axis top coordinates
#' @param border_size A numeric, bbox margin
#' @param prob Probability to inflate a filed margin
#' @param mean_thickness Margin width expectation
#' @param v_thickness Margin width variance
#' 
#' @return A \link[sp]{SpatialPolygonsDataFrame} object with n fields, prop pourcentage of toxic fields.
#' 
#' @examples \dontrun{
#' land <- simulateLandscape(n=100, prop=0.4, range=10,
#' xmin=0, xmax=1000, ymin=0, ymax=1000, border_size=100,
#' prob=runif(1,0.1,0.9), mean_thickness=runif(1,2,20),
#' v_thickness=50)
#' plot(land) }
#' 
#' @seealso \link{simulateInitialPartition} and \link{simulateThickMargins}
#' 
#' @export
#' 
simulateLandscape <-
  function(n = 500,
           prop = 0.4,
           range = 10,
           xmin = 0,
           xmax = 5000,
           ymin = 0,
           ymax = 5000,
           border_size = 200,
           prob = runif(1, 0.1, 0.9),
           mean_thickness = runif(1, 2, 20),
           v_thickness = 50) {
    objectLTemp <- 
      simulateInitialPartition(
        n = n,
        prop = prop,
        range = range,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      )
    objectL <- simulateThickMargins(objectLTemp, border_size, prob, mean_thickness, v_thickness)
    return(objectL)
  }

#' @title Simulate thick margin to a landscape
#' 
#' @name simulateThickMargins
#' 
#' @description Simulate thick margins as receptors in a landscape.
#' 
#' @details Margin width use a Gamma distribution with shape and scale parameters based on thickness mean and variance.
#' 
#' @importFrom methods slot
#' @importFrom sf as_Spatial
#' @importFrom rgeos gBuffer
#' 
#' @param objectL sf, sp or Landscape (earlier version of briskaR).
#' @param border_size A numeric, bbox margin
#' @param prob Probability to inflate a margin
#' @param mean_thickness Margin width expectation in meter
#' @param v_thickness Margin width variance in meter
#' 
#' @return A \link[sp]{SpatialPolygonsDataFrame} object
#' 
#' @seealso \link{simulateInitialPartition} and \link{simulateLandscape}
#' 
#' @examples
#' \dontrun{
#' data(maize_65)
#' plot(maize.landscape)
#' landscape.margin <- simulateThickMargins(maize.landscape)
#' plot(landscape.margin) }
#' 
#' @export
#' 
simulateThickMargins <- function(objectL,
                                 border_size = 200,
                                 prob = runif(1, 0.1, 0.9),
                                 mean_thickness = runif(1, 2, 20),
                                 v_thickness = 50){
  
    if("sf" %in% class(objectL)){
      objectL <- sf::as_Spatial(objectL)
    }
    
    bbox_xmin <- objectL@bbox["x","min"]
    bbox_xmax <- objectL@bbox["x","max"]
    bbox_ymin <- objectL@bbox["y","min"]
    bbox_ymax <- objectL@bbox["y","max"]
  
    coor <-
      data.frame(
        "x" = c(
          bbox_xmin + border_size,
          bbox_xmax - border_size,
          bbox_xmax - border_size,
          bbox_xmin + border_size,
          bbox_xmin + border_size
        ),
        "y" = c(
          bbox_ymax - border_size,
          bbox_ymax - border_size,
          bbox_ymin + border_size,
          bbox_ymin + border_size,
          bbox_ymax - border_size
        )
      )
    
    c <-
      data.frame(
        "x" = c(
          bbox_xmin - 200,
          bbox_xmax + 200,
          bbox_xmax + 200,
          bbox_xmin - 200,
          bbox_xmin - 200
        ),
        "y" = c(
          bbox_ymax + 200,
          bbox_ymax + 200,
          bbox_ymin - 200,
          bbox_ymin - 200,
          bbox_ymax + 200
        )
      )
    
    # set a box
    hole <- Polygon(coor, hole = TRUE)
    o <- Polygon(c, hole = FALSE)
    geom <- Polygons(list(o, hole), "box")
    box <- SpatialPolygons(list(geom))
    
    # Get border
    segments <- edgeslines(objectL) #on récupère les segments (bordures) à partir du pavage
    nsegment <- length(segments@lines)
    
    bernoulli <- rbinom(nsegment, 1, prob) # on tire dans la binomiale la proba d'avoir une largeur n
    margin_width <-
      rgamma(
        nsegment,
        shape = mean_thickness * mean_thickness / v_thickness,
        rate = mean_thickness / v_thickness
      ) #largeur des bordures tirée dans une gamma
    
    length_margin <-  bernoulli * margin_width #largeur des bordures de champ
    #length_margin[which(length_margin==0)]<-1e-1
    
    # get polygons ID
    ids = sapply(slot(objectL, "polygons"), function(x)
      slot(x, "ID"))
    
    #create margins
    seg_margin <-
      gBuffer(segments,
              byid = TRUE,
              width = length_margin / 2,
              capStyle = "ROUND") # inflated segments
    
    margin <- gUnionCascaded(seg_margin) # inflated segments union in 1 polygon
    margin@bbox <- objectL@bbox
    field_margin <- gDifference(margin, box, byid = T)
    field_margin@bbox <- objectL@bbox
    proj4string(field_margin) <-  proj4string(objectL)
    margin_ids = as.character(seq(
      max(as.numeric(ids)) + 1,
      max(as.numeric(ids)) + length(field_margin@polygons)
    ))
    nb_margins = length(field_margin@polygons)
    
    fields <-
      gDifference(objectL,
                  field_margin,
                  byid = TRUE,
                  id = ids)
    fields@bbox <- objectL@bbox
    fields_data <- objectL@data[sapply(fields@polygons, slot, "ID"), ]
    
    receptors <- rep(0, nrow(fields_data) + nb_margins)
    #receptors[(as.numeric(margin_ids)+1)]=1
    receptors[(nrow(fields_data) + 1):(nrow(fields_data) + nb_margins)] <-  1
    
    # TODO extraire les polygons et dataframe de fields#
    
    # change margins ID
    for (mp in 1:nb_margins) {
      field_margin@polygons[[mp]]@ID = margin_ids[mp]
    }
    # Add margin to polygons landscape
    temp_landscape <-
      SpatialPolygons(c(fields@polygons, field_margin@polygons))
    
    new_data_frame <-
      cbind(rbind(
        fields_data,
        data.frame(
          "sources" = rep(0, nb_margins),
          "neutral" = rep(0, nb_margins),
          row.names = margin_ids[]
        )
      ), "receptors" = receptors)
    
    new_landscape <- SpatialPolygonsDataFrame(temp_landscape, new_data_frame)
    new_landscape@bbox <- objectL@bbox
    proj4string(new_landscape) <- proj4string(objectL)
    
    # objectL <- new_landscape
    objectL <- st_as_sf(new_landscape)
    
    return(objectL)
  }

#' Wrapper function : loadLandscape
#' @name loadLandscape
#'
#' @description Wrapper function to create a Landscape object using SpatialPolygons and dataframe.
#' The SpatialPolygons object and the data.frame have to contain the same number of polygons
#'  and row (row ID is polygons ID).
#'
#' @param sp a SpatialPolygons object designing the landscape
#' @param data a data.frame containing fields (polygons) information. Row names as fields ID,
#'  column names as sources | neutral | receptors (for a given field, the value is 1 for the
#'   type of the field (source or neutral or receptor), otherwise 0).
#'   
#' @return A \link[sp]{SpatialPolygonsDataFrame} object
#' 
#' @examples
#' data(maize_65)
#' maize_data <- maize_65@data
#' maize_sp_only <- maize_65 ; maize_sp_only@data = data.frame(remove = rep(0,nrow(maize_65@data)))
#' load_landscape <- loadLandscape(maize_sp_only, maize_data)
#' 
#' @export
loadLandscape <- function(sp, data) {

  sptemp <- spTransform(sp, CRSobj = CRS(.briskar_env$BRISKAR_INTERN_PROJECTION))
  newsp <- SpatialPolygons(sptemp@polygons)
  proj4string(newsp) <- proj4string(sptemp)
  
  df <- subset(data,
               select = which(
                 colnames(data) == "sources" |
                 colnames(data) == "neutral" |
                 colnames(data) == "receptors") )
  
  thelandscape <- SpatialPolygonsDataFrame(newsp, data = df, FALSE)
  thelandscape@bbox <- sptemp@bbox
  thelandscape@plotOrder <- sptemp@plotOrder
  
  return(st_as_sf(thelandscape))
}

#' Create a Landscape object from SIG shapefile file
#' 
#' @name loadLandscapeSIG
#'
#' @description Create a Landscape object from SIG shapefile. Shapefile has to contain a SpatialPolygonsDataFrame.
#'  Data in the data frame contain fields (polygons) information. Row names as fields ID, cols
#'  names as sources | neutral | receptors (for a given field, the value is 1 for the type of the field
#'  (source or neutral or receptor), otherwise 0).
#'
#' @rdname Landscape-load-sig-class
#' 
#' @importFrom rgdal readOGR
#' 
#' @param dsn folder path to the source files
#' @param layer file name without extension
#' @param format only load data needed Landscpae-class (default TRUE)
#' 
#' @return A \link[sp]{SpatialPolygonsDataFrame} object
#' 
#' @examples \dontrun{
#' land <- loadLandscapeSIG("/path/to/directory/","fileName")
#' plot(land)
#' }
#' 
#' @export
#' 
loadLandscapeSIG <- function(dsn, layer, format = TRUE) {
  if (requireNamespace("rgdal", quietly = T)) {
    ogr <- readOGR(dsn, layer)

    thelandscape <- spTransform(ogr, CRSobj = CRS(.briskar_env$BRISKAR_INTERN_PROJECTION))

    
    if (format == TRUE) {
      df <- subset(thelandscape@data,
                   select = which(
                     colnames(thelandscape@data) == "sources" |
                       colnames(thelandscape@data) == "neutral" |
                       colnames(thelandscape@data) == "receptors" ) )
      thelandscape@data <- df
    }
    else {
      warning(
        "Object Landscape will not be compatible to briskaR package functions\n Use option format=TRUE\n"
      )
    }
    
    return(st_as_sf(thelandscape))
  }
  else {
    warning("loadLandscapeSIG function need \"rgdal\" package")
    return(NULL)
  }
  
}


#' Save Particles Dispersion 3D Array to tiff file
#' 
#' @name saveIntoFile
#'
#' @description Save into tiff file particles dispersion 3D array from toxicIntensity.
#' The output is a RasterStack with a layer per time unit with projection set to CRS="+proj=longlat +datum=WGS84"
#' 
#' @rdname Landscape-save-tiff
#' 
#' @importFrom raster addLayer extent setExtent extent<- projectRaster writeRaster 
#' 
#' @param objectL a \code{Landscape} object
#' @param objectT a 3D array particles dispersion indexed by time (output from \link{toxicIntensity})
#' @param filename output file name (default "ParticlesDispersion.tif")
#' @param format output format (default=GTiff)
#' @param overwrite if TRUE overwrite file (default TRUE)
#' 
#' @return a RasterStack object
#' 
#' @examples \dontrun{
#' data(maize_65)
#' ti <- toxicIntensity(maize.landscape,maize.emitted_pollen)
#' saveIntoFile(maize.landscape,ti,filename="ParticlesDispersion.tiff",format="GTiff",overwrite=T)
#' }
#' 
#' @export
#' 
saveIntoFile <-
  function(objectL,
           objectT,
           filename = "ParticlesDispersion.tif",
           format = "GTiff",
           overwrite = TRUE) {
    timemax = length(objectT[, 1, 1])
    
    r <- raster(as.matrix(objectT[1, , ]), crs = objectL@proj4string)
    #extent(r)<-extent(objectL)
    names(r) <- paste("time", as.character(1), sep = ".")
    
    for (t in 2:timemax) {
      rtemp <- raster(as.matrix(objectT[t, , ]), crs = objectL@proj4string)
      #extent(rtemp)<-extent(objectL)
      names(rtemp) <- paste("time", as.character(t), sep = ".")
      r <- addLayer(r, rtemp)
    }
    extent(r) <- extent(objectL)
    
    r <- projectRaster(r, crs = "+proj=longlat +datum=WGS84")
    
    rf <- writeRaster(r,
                      filename = filename,
                      format = format,
                      overwrite = overwrite)  #bylayer=TRUE
    
    return(rf)
    
  }

#################
#### PRIVATE ####
#################

### create.voronoi.points : fonction qui simule les graines pour le pavage de voronoi avec des marques OGM NonOGM
## pour contrôler l'agregation spatiale des points OGM on utilise un processus gaussien avec autocorrélation spatiale
## ici on utilise une covariance exponentielle.
# L'idée étant de faire une analyse de sensibilité on fixe le paramètre phi et on fait varier l'étendue r
##prend en paramètre le nombre de graines n, la proportion ogm prop, xmin xmax ymin ymax, le paramètre de porté de la covariance covar
create.voronoi.points <- function(n, prop, range, xmin, xmax, ymin, ymax) {
  coor <- cbind(runif(n, xmin, xmax), runif(n, ymin, ymax))
  d <- as.matrix(dist(coor))
  cov_mat <- Exponential(d, range = range, phi = 10)
  s <- MASS::mvrnorm(1, mu = rep(0, n), Sigma = cov_mat)
  n_ogm = floor(prop * n) #nombre de parcelles OGM
  data <- as.data.frame(s)
  sources <- rep(0, n)
  neutral <- rep(0, n)
  threshold <- max(sort(data$s)[1:n_ogm])
  sources[which(s <= threshold)] = c(1) #marque source (OGM) = 1 dans la colonne sources du dataframe
  neutral[which(s > threshold)] = c(1) #marque source neutre = 1 dans la colonne neutral du dataframe
  f <- SpatialPointsDataFrame(coords = coor, data = cbind.data.frame(sources, neutral))
  return(f)
}

## vornoi retourne un objet SpatialPolygonsDataFrame correspondant à un pavage de voronoi
#layer= SpatialPointsDataFrame, xmin,xmax, ymin, ymax= bornes du domaine/paysage
#' @importFrom deldir deldir tile.list
create.voronoi.diagram <- function(layer, xmin, xmax, ymin, ymax) {
  crds = layer@coords     #extraction de la matrice des coordonn?es de l'objet layer
  z = deldir::deldir(crds[, 1],
                     crds[, 2],
                     rw = c(xmin, xmax, ymin, ymax),
                     suppressMsge = T) # triangulation en fonction des points proposés
  w = deldir::tile.list(z)      #pour chaque point-centre extrait le polygone-pavage
  polys = vector(mode = 'list', length = length(w))
  for (i in seq(along = polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1, ])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  SP = SpatialPolygons(polys)
  SP@bbox[, 1] <- c(xmin, ymin)
  SP@bbox[, 2] <- c(xmax, ymax)
  voronoi = SpatialPolygonsDataFrame(SP, data = layer@data)
  return (voronoi)
}

## edgeslines : à partir d'un objet polygons, retourne un SpatialLines (mieux ordonné qu'avec as(,SpatialLines))
edgeslines = function(voronoi) {
  mat = matrix(c(0, 0, 0, 0), ncol = 4, byrow = TRUE)
  for (i in seq(voronoi)) {
    coor = voronoi@polygons[[i]]@Polygons[[1]]@coords
    for (j in seq(2, nrow(coor)))  {
      if (coor[j - 1, 1] < coor[j, 1]) {
        mat = rbind(mat, c(coor[j - 1, ], coor[j, ]))
      } else {
        mat = rbind(mat, c(coor[j, ], coor[j - 1, ]))
      }
    }
  }
  mat = unique(mat[-1, ])
  edges = vector(mode = 'list', length = nrow(mat))
  for (i in seq(nrow(mat))) {
    edges[[i]] = Lines(Line(coords = rbind(mat[i, 1:2], mat[i, 3:4])), ID =
                         as.character(i))
  }
  SE = SpatialLines(edges)
}

# Remove dependency to fields package !
Exponential <- function(d, range = 1, alpha = 1/range, phi = 1.0) {
  #
  # Matern covariance function transcribed from Stein's book page 31
  # nu==smoothness==.5, alpha ==  1/range
  # check for negative distances
  if (any(d < 0)) 
    stop("distance argument must be nonnegative")
  #
  return(phi*exp(-d * alpha))
}

