#' @title Sampling based on distribution provided by Raster* object over a sf object
#' 
#' @description Sampling based on distribution provided by Raster* object 
#' (does not have to be summed to one) over an sf object used as mask
#' 
#' 
#' @param rasterStack Raster* object (RasterLayer or RasterStack)
#' @param sf An sf object
#' @param sizeSite scalar or vector of the number of individual to sample per site. Default is 1.
#' If vector, as to be the length of Raster* object
#' 
#' @importFrom raster mask getValues nlayers
#' @importFrom sf st_as_sf st_crs
#' 
#' @export
#' 
brk_sampling = function(rasterStack, sf, sizeSite = 1){
  if(length(sizeSite) == 1){ sizeSite = rep(sizeSite, raster::nlayers(rasterStack))}
  if(!(length(sizeSite) %in% c(1, raster::nlayers(rasterStack)))){
    stop("length of sizeSite must be scalar or vector of length equalm to the number of layers of rasterStack")
  }
  # 1. crop/mask the raster area to used
  rasterMask = raster::mask(rasterStack, as_Spatial(sf)) # MASK DOES NOT WORK FOR MARGINS !!!  
  #2. get values
  rasterMask_val = raster::getValues(rasterMask)
  # 3. sample in each column a new coordinates
  cellNum = sapply(1:ncol(rasterMask_val),
                   function(i){
                     cellNumVector = 1:length(rasterMask_val[,i])
                     cellNumVector_NoNA = cellNumVector[!is.na(rasterMask_val[,i])]
                     rasterMask_val_NoNA = rasterMask_val[,i][!is.na(rasterMask_val[,i])]
                     return(sample(x = cellNumVector_NoNA, size = sizeSite[i], replace = FALSE, prob = rasterMask_val_NoNA))
                   })
  ls_coordPoints = lapply(1:nlayers(rasterMask), function(i){ raster::xyFromCell(rasterMask, cellNum[i]) })
  coordPoints = do.call("rbind", ls_coordPoints)
  # 4. convert coors of points to SF
  sfReturn = sf::st_as_sf(x = as.data.frame(coordPoints),                         
                          coords = c("x", "y"),
                          crs = sf::st_crs(sf))
  return(sfReturn)
}