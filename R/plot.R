#' @title Plot from raster and sp (sf)
#'
#' @description  Re-exported \code{plot} method from `raster`, `sp` and `sf`
#' packages to avoid failing when trying to plot `raster` or `sf` objects without
#'  having `raster` or `sf` loaded first.
#'  
#' @note For \link[sf]{sf} objects, the package `ggplot2` include the function `geom_sf` 
#'  
#' @name plot
#' @rdname plot
#' @keywords internal
#' @export
#' @importFrom raster plot
#' @importFrom sp plot
NULL