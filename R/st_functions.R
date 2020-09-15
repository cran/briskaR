#' @title Get, set, or replace geometry from an `sf`` object
#'
#' @description  Compute Euclidian or great circle distance
#'  between pairs of geometries; compute, the area or the
#'  length of a set of geometries.
#'  
#'  
#' @name st_geometry
#' @rdname st_geometry
#' @keywords internal
#' @export
#' @importFrom sf st_geometry
NULL

#' @title Compute geometric measurements
#'
#' @description  Compute Euclidian or great circle distance
#'  between pairs of geometries; compute, the area or the
#'  length of a set of geometries.
#'  
#' @name st_length
#' @rdname st_length
#' @keywords internal
#' @export
#' @importFrom sf st_length
NULL

#' @title Compute geometric measurements
#'
#' @description  Compute Euclidian or great circle distance
#'  between pairs of geometries; compute, the area or the
#'  length of a set of geometries.
#'  
#' @name st_area
#' @rdname st_area
#' @keywords internal
#' @export
#' @importFrom sf st_area
NULL

#' @title Create sf object
#'
#' @description  Create sf, which extends data.frame-like objects with a simple feature list column
#'  
#' @name st_sf
#' @rdname st_sf
#' @keywords internal
#' @export
#' @importFrom sf st_sf
NULL

#' @title sample points on or in (sets of) spatial features
#'
#' @description Sample points on or in (sets of) spatial features.
#' By default, returns a pre-specified number of points that is equal to size
#' (if type = "random") or an approximation of size (for other sampling types).
#'  
#' @name st_sample
#' @rdname st_sample
#' @keywords internal
#' @export
#' @importFrom sf st_sample
NULL

#' @title get nearest points between pairs of geometries
#'
#' @description get nearest points between pairs of geometries
#'  
#' @name st_nearest_points
#' @rdname st_nearest_points
#' @keywords internal
#' @export
#' @importFrom sf st_nearest_points
NULL

#' @title union feature geometries
#'
#' @description union feature geometries
#'  
#' @name st_union
#' @rdname st_union
#' @keywords internal
#' @export
#' @importFrom sf st_union
NULL

#' @title A set of function imported from `raster` package
#'
#' @description  Re-exported \code{nlayers} method from `raster`
#' package to avoid failing when working on `raster` objects without having 
#' `raster` loaded first.
#'  
#' @name nlayers
#' @rdname nlayers
#' @keywords internal
#' @export
#' @importFrom raster nlayers
NULL

#' @title A set of function imported from `raster` package
#'
#' @description  Raster to points conversion. Cells with NA are not converted. A function
#' can be used to select a subset of the raster cells (by their values).
#'  
#' @name rasterToPoints
#' @rdname rasterToPoints
#' @keywords internal
#' @export
#' @importFrom raster rasterToPoints
NULL


#' @title A set of function imported from `raster` package
#'
#' @description  Set values of a `Raster*`` object
#'  
#' @name setValues
#' @rdname setValues
#' @keywords internal
#' @export
#' @importFrom raster setValues
NULL


#' @title Test if an \link[sf]{sf} is a square
#' 
#' @description  Test if an sf is a square, with tolerance. Default is 5% tolerance.
#' 
#' @param .sf and object of class \link[sf]{sf}
#' @param tolerance tolerance rate between both square side length
#' 
#' @export
#' 
is_square_sf <- function(.sf, tolerance=0.05){
  bbox <- st_bbox(.sf)
  x_length <- as.numeric(abs(abs(bbox$xmax) - abs(bbox$xmin)))
  y_length <- as.numeric(abs(abs(bbox$ymax) - abs(bbox$ymin)))
  limit_min <- (x_length + y_length)/2 * (1-tolerance)
  limit_max <- (x_length + y_length)/2 * (1+tolerance)
  
  return(all(x_length <= limit_max, limit_min <= x_length,
             y_length <= limit_max, limit_min <= y_length)
  )
}


#' @title Add squared frame polygon
#' 
#' @description Return a square frame surrounding a list of sf
#' 
#' @param list_sf A list of objects of class \link[sf]{sf} 
#' @param buffer_dist numeric; buffer distance for all, or for each of the elements in x;
#'  in case dist is a units object, it should be convertible to arc_degree
#'   if x has geographic coordinates, and to st_crs(x)$units otherwise. 
#'   See function `sf_buffer` from package `sf` for details
#' 
#' @export
#' 
st_squared_geometry <- function(list_sf, buffer_dist = NULL){
  if(length(list_sf)!=1){
    if(!do.call(all.equal, lapply(list_sf, function(i) st_crs(i)$epsg))){
      stop("coordinate reference system differ between elements of list")
    }
  }
  map_bbox <- sapply(list_sf, st_bbox)
    
  map_lat_min <- min(map_bbox["ymin",])
  map_lat_max <- max(map_bbox["ymax",])
  map_lon_min <- min(map_bbox["xmin",])
  map_lon_max <- max(map_bbox["xmax",])
  
  if(map_lat_max-map_lat_min > map_lon_max-map_lon_min){
    # longest side = latitude
    map_lon_mid <- map_lon_max - (map_lon_max-map_lon_min)/2
    map_lon_min <- map_lon_mid - (map_lat_max-map_lat_min)/2
    map_lon_max <- map_lon_mid + (map_lat_max-map_lat_min)/2
  } else{
    # longest side = lonitude
    map_lat_mid <- map_lat_max - (map_lat_max-map_lat_min)/2
    map_lat_min <- map_lat_mid - (map_lon_max-map_lon_min)/2
    map_lat_max <- map_lat_mid + (map_lon_max-map_lon_min)/2
  }
  
  # For polygon, we need to close it: first and last points must be identical!!!
  df <- data.frame(lon = c(map_lon_min, map_lon_min, map_lon_max, map_lon_max, map_lon_min),
                   lat = c(map_lat_min, map_lat_max, map_lat_max, map_lat_min, map_lat_min))
  
  frame <- st_sfc(st_polygon(list(as.matrix(df))), crs = st_crs(list_sf[[1]])$proj4string)
  sf_frame <- sf::st_sf(geometry=frame)
  if(!is.null(buffer_dist)){
    sf_frame <- st_buffer(sf_frame, dist = buffer_dist)
  }
  return(sf_frame)
}

#' @title Simulate thick margin to a landscape
#' 
#' @description Add buffer around each objects of a \link[sf]{sf} file
#' 
#' @details see package \link[sf]{st_buffer} for details
#' 
#' @importFrom sf st_buffer st_difference st_union
#' @importFrom graphics layout par plot.new legend rect
#' @importFrom grDevices rgb heat.colors colorRampPalette
#' 
#' @param sf object of class sfg, sfg or sf
#' @param dist numeric; buffer distance for all, or for each of the elements in x;
#'  in case dist is a units object, it should be convertible to arc_degree if x
#'   has geographic coordinates, and to st_crs(x)$units otherwise
#' @param nQuadSegs integer; number of segments per quadrant (fourth of a circle), for all or per-feature
#' @param endCapStyle character; style of line ends, one of 'ROUND', 'FLAT', 'SQUARE'
#' @param joinStyle character; style of line joins, one of 'ROUND', 'MITRE', 'BEVEL'
#' @param mitreLimit numeric; limit of extension for a join if joinStyle 'MITRE' is used (default 1.0, minimum 0.0)
#' 
#' @export
#' 
st_multibuffer <- function(sf,
                           dist = 50,
                           nQuadSegs = 30, endCapStyle = "ROUND",
                           joinStyle = "ROUND", mitreLimit = 1) {
  
  # NOTE: dist can be a vector
  if(length(dist) == nrow(sf)){
    k <- lapply(1:nrow(sf), function(i) st_buffer(x = sf[i,], dist[i], nQuadSegs, endCapStyle, joinStyle, mitreLimit) )
  } else if (length(dist) == 1){
    k <- lapply(1:nrow(sf), function(i) st_buffer(x = sf[i,], dist, nQuadSegs, endCapStyle, joinStyle, mitreLimit) )
  } else{
    stop("Argument 'dist' must be a vector of length 1 or of the nrow of 'sf'.")
  }
  
  join_k <- do.call('rbind', k)
  
  # set difference: need to keep only geometry to avoid warnings
  join_k <- join_k[,"geometry"]
  geom_sf <- sf[,"geometry"]
  
  # set difference
  l <- lapply(1:nrow(sf), function(i){
    j = ifelse(length(dist) == 1, 1, i)
    if(dist[j] > 0 ){
      st_difference(join_k[i,], geom_sf[i,]) 
    } else {
      st_difference(geom_sf[i,], join_k[i,])
    }
  })
  join_l <- do.call('rbind', l)
  # union to avoid ovelapping of merged part
  union <- st_union(join_l)
  sf_union <- sf::st_sf(geometry=union)
  return(sf_union)
}