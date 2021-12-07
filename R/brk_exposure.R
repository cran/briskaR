#' @title Simulate new points on a specific sf object
#' 
#' @description Simulate new points on a specific \link[sf]{sf} object. See \link[sf]{st_sample} for details.
#' 
#' 
#' @param sf object of class sf or sfc
#' @param size sample size(s) requested; either total size, or a numeric vector with sample sizes for each feature geometry. 
#' When sampling polygons, the returned sampling size may differ from the requested size, as the bounding box is
#'  sampled, and sampled points intersecting the polygon are returned.
#' 
#' @export
#' 
brk_newPoints <- function(sf, size = 100){
  ls_points <- st_sf( geometry = st_sample(sf, size = size, type = "random", exact = TRUE))
  return(ls_points)
}

#' @title Add sequence to element of a \link[sf]{sf} object
#' 
#' @description Add column with sequence for each row to a \link[sf]{sf} object
#' 
#' @param sf object of class sf or sfc
#' @param key a character string used as name of the new column
#' @param from the starting value of the sequence. Of length 1 unless just from is supplied as an unnamed argument.
#' @param to the end (maximal) value of the sequence. Of length 1 unless just from is supplied as an unnamed argument.
#' @param by number: increment of the sequence.
#' 
#' 
#' @export
#' 
brk_timeline <- function(sf, key, from, to, by){
  if(length(from)==1) from = rep(from, nrow(sf))
  if(length(to)==1) to = rep(to, nrow(sf))
  sf[[key]] <- lapply(1:nrow(sf), function(i) return(seq(from[i], to[i], by)))
  return(sf)
}


#' @title Add raster value to element of sf object
#' 
#' @description Add the raster value(s) of a Raster* object to element of sf object.
#' 
#' @param stackRaster_exposure The Raster* object
#' @param sf the sf object
#' @param stackTimeline sequence with matching to elements of RasterStack (length of stackTimeline
#'  must be the same as length of a list of stackRaster_exposure)
#' @param keyTime name of the column to match exposure timeline from stackRaster_exposure
#' @param key name of the new column
#' 
#' @export
#' 
brk_exposureMatch <- function(stackRaster_exposure, sf, stackTimeline, keyTime = "TIMELINE", key = "EXPOSURE"){
  
  # 1. Get the list of exposure of each point over the timeline of stackexposure 
  list_raster_exposure <- raster::as.list(stackRaster_exposure)  
  
  if(length(list_raster_exposure) != length(stackTimeline)){
    stop("number of Raster in stackRaster_exposure and length of stackTimeline mismatch")
  }
  
  ls_EXPOSURE <- 
    lapply(1:length(list_raster_exposure),
           function(i){
             raster::extract(x = list_raster_exposure[[i]], y = sf)
           })
  
  # 2. get index matching between stackTimeline each individuals timeline
  index_matching <- lapply(1:nrow(sf),
                           function(i){
                             match(sf[[keyTime]][[i]], stackTimeline) # ATTENTION, ORDER MATTER !!!
                           })
  # Note: Since `NA` may appear in index_matching when timeline of individuals is longer than stackTimeline
  # we have to clean timeline element where exposure is modified !
  if(any(lapply(index_matching, function(x) any(is.na(x))) == TRUE)){
    warning("Note that some elements in column ", keyTime, " does not match with stackTimeline. So the column ", keyTime, " has changed.")
    sf[[keyTime]] = lapply(1:nrow(sf), function(i) sf[[keyTime]][[i]][which(!is.na(index_matching[[i]]))] )
  }
  
  # 3. add exposure to individual timeline
  sf_EXPOSURE <-
    lapply(1:nrow(sf),
           function(i){
             unlist(lapply(ls_EXPOSURE, `[[`, i)[index_matching[[i]]])
           })
  
  sf[[key]] <- sf_EXPOSURE
  
  return(sf)
}


#' @title Compute exposure for a `RasterStack` class object from package `raster`
#'
#'
#' @details Local intensity depends of \code{beta} and \code{alpha} parameters.
#' Beta represents the toxic adherence between [0,1].
#' Alpha represents a list of parameters of the lost of toxic particules
#' due to covariates (precipitation).
#' There are two configurations to integrate the loss in the function :
#' (i) simulating covariate (simulate=TRUE) or (ii) uploading covariate (simulate=FALSE).
#' The covariate is linked to the loss by a linear regression with paramaters minalpha,
#' maxalpha, covariate_threshold.
#' 
#' @importFrom utils flush.console
#' 
#' @param RasterStack_dispersal RasterStack. An object of class\code{RasterStack} (see package \code{raster} for details).
#' @param sf sf. And object of class `sf` on which exposure is computed from the previous
#' list of raster by patch `RasterStack_dispersal`. See \link[sf]{sf} for details.
#' @param key name of the column in `sf` object providing emission amount
#' @param keyTime name o the column of sf for time
#' @param loss numeric. scalar or vector (of the same length as the number the timeline include is argument sf) to apply a loss on exposure cells.
#' @param beta numeric. toxic adherence parameter between 0 and 1 (default = 0.4).
#' @param nbr_cores integer. Set the number of cores to used for parallel computing.
#' @param quiet boolean. Set `TRUE` to remove progress bar.
#' @param unit default is meter \code{"m"}. but should be more generic: "any".
#' 
#' @export
#' 
brk_exposure <- function(RasterStack_dispersal,
                         sf,
                         key,
                         keyTime,
                         loss, ### to be removed once well integrated !!!
                         beta,
                         nbr_cores,
                         quiet,
                         unit){
  UseMethod("brk_exposure")
}

#' @export
#' 
brk_exposure.RasterStack <- function(RasterStack_dispersal,
                                     sf,
                                     key,
                                     keyTime = NULL,
                                     loss = NULL,
                                     beta = 0.4,
                                     nbr_cores = 1,
                                     quiet=FALSE,
                                     unit = "m"){
  
  # should also check ID?
  nbr_RasterStack <- length(RasterStack_dispersal@layers)
  nbr_patch <- nrow(sf)
  
  if(nbr_RasterStack != nbr_patch){
		stop("Number of stack layers and sf objects differ.")
  }
  
  if(is.null(keyTime)){stop("please provide a column name of 'sf' refering to the timeline.")}
  
  if(colListEqual(sf[[keyTime]])){
    timeline <- sf[[keyTime]][[1]] # all patch have the same timeline!
  } else{
    stop(paste("element of column", keyTime, " in object 'sf' are not all equal."))
  }
  #-------------------------- BEGIN FUNCTION
  # perte sur les feuilles de plante hôte : dépend du temps et des conditions météo
  # fonction simulateur stochastique de climat :(attention chemin vers data de climat dans la fonction)
  if(is.null(loss)){
    loss = rep(0, length(timeline)) # all patch have the same timeline!
  }
  # ------------------------- END LOSS FUNCTION
  
  # --- cell surface
  bbox <- RasterStack_dispersal@extent
  size_raster <- RasterStack_dispersal@ncols # should be equal to RasterStack_dispersal@ncols
  if(unit == "m"){
    cell_area <- ((bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)) / ( RasterStack_dispersal@ncols * RasterStack_dispersal@nrows)
  } else{
    cell_area <- raster::area(RasterStack_dispersal)[1] * 10^6 # convert km^2 to m^2
  }
  
  
  message("Step 1/2: Compute global spatio-temporal exposure profile... ", appendLF = T)
  #sf <- base::as.list(dplyr::select(sf, -patch_id))
  
  list_matrix_convol <- lapply(1:nbr_RasterStack, function(i){ raster::as.matrix(RasterStack_dispersal[[i]]) })
  
  out_matrix_exposure <- sptp_exposure(sf = sf,
                                       list_matrix_convol = list_matrix_convol,
                                       key = key,
                                       timeline = timeline,
                                       size_raster = size_raster,
                                       loss = loss,
                                       cell_area = cell_area,
                                       beta = beta,
                                       quiet = quiet)
  
  
  
  message("done ", appendLF = T)
  message("Step 2/2: Convert all matrices to rasters... ", appendLF = F)
  
  RasterStack_exposure <- lapply(1:length(out_matrix_exposure), function(i){
    raster::raster(x = out_matrix_exposure[[i]],
                   crs = RasterStack_dispersal@crs,
                   xmn = bbox@xmin,
                   xmx = bbox@xmax,
                   ymn = bbox@ymin,
                   ymx = bbox@ymax)
    
  })
  RasterStack_exposure <- raster::stack(RasterStack_exposure)
  #names(RasterStack_exposure) <- paste0(timeline)
  names(RasterStack_exposure) <- as.factor(timeline)
  
  message("done ", appendLF = TRUE)
  return(RasterStack_exposure)
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
############## PRIVATE ########################################################
###############################################################################
# local exposure
sptp_exposure <- function(sf,
                          list_matrix_convol,
                          key,
                          timeline,
                          size_raster,
                          loss,
                          cell_area,
                          beta,
                          quiet=FALSE){
  
  ls_matrix = list() # list is faster, and it allows to name each matrix 
  
  size_raster <- ncol(list_matrix_convol[[1]]) # all matrix have the same size!
  if(length(loss) == 1){
    loss = rep(loss, length(timeline))
  }
  if(length(loss) != length(timeline)){
    stop("length  of 'loss' vector is different of the length of timeline")
  }
  
  for (time_id in 1:length(timeline)) {
    
    matrix_toxic <- matrix(data = 0, nrow = size_raster, ncol = size_raster)
    
    for (patch_id in 1:length(list_matrix_convol)) {
      if (sf[[key]][[patch_id]][time_id] != 0) {
        matrix_toxic <- matrix_toxic + (cell_area * sf[[key]][[patch_id]][time_id] * beta * list_matrix_convol[[patch_id]] )
      }
    }
    if (time_id == 1) {
      ls_matrix[[time_id]] <- matrix_toxic
    } else  {
      ls_matrix[[time_id]] <- matrix_toxic + ls_matrix[[time_id - 1]] * (1 - loss[time_id]) 
    }
    
    if(quiet != TRUE){
      seq_print <- 1:length(timeline)/length(timeline) * 10
      # cat(paste0(time_id, "/", length(timeline), "..."))
      cat('\r', "|", rep("*", round(seq_print[time_id])), rep("_", 10-round(seq_print[time_id])))
      flush.console()
    }
  }
  names(ls_matrix) <- timeline
  
  return(ls_matrix)
}

# -------- internal 
colListEqual = function(colList){
  all(sapply(1:length(colList), function(i) all.equal(colList[[1]], colList[[i]])))
}
