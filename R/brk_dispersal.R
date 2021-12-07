#' @title Compute dispersal
#' 
#' @description Simulate contaminants or individuals frequency over the landscape by two
#' steps: dispersal of elements and local intensity/frequency of elements after dispersal.
#'
#' @details The dispersal of contaminants or individuals is implemented by rastering
#' the landscape and by computing the convolution between sources emissions
#' and a dispersal kernel.
#' 
#' The dispersion kernel by default is Normal Inverse Gaussian kernel
#'  ("NIG" function). Currently, two others are implemented "geometric"
#'  (with parameter \code{a}) and "2Dt" kernels (with parameters \code{a},
#'  \code{b}, \code{c1}, \code{c2}).
#'
#' @name brk_dispersal
#' 
#' @param object sf or patialPolygonsDataFrame. A simple feature of class \link[sf]{sf} or \link[sp]{SpatialPolygonsDataFrame}
#' @param size_raster integer. Raster size (default = 2^10)
#' @param tolerance_square numeric. Tolerance rate to test if an sf set is squared
#' @param kernel string. Dispersion kernel, function name (default = NIG)
#' @param kernel.options list. Parameters list for the kernel function
#' @param nbr_cores integer. Parameters for parallel computing: the
#' number of cores to use, i.e. at most how many child processes
#' will be run simultaneously. Default is \code{1} (non parallel).
#' @param squared_frame sf. Select the sf to be considered as frame to rasterized.
#' Default is `NULL`, and `object` is used. 
#' 
#' 
#' @export 
#' 
brk_dispersal <- function(object,
                          size_raster,
                          tolerance_square,
                          kernel,
                          kernel.options,
                          nbr_cores,
                          squared_frame){
  UseMethod("brk_dispersal")
}

#' @importFrom sf st_sf
#' 
#' @export
#' 
brk_dispersal.sfc <- function(object,
                              size_raster = 2^10,
                              tolerance_square = 0.1,
                              kernel = "NIG",
                              kernel.options = list(
                                "a1" = 0.2073 ,
                                "a2" = 0.2073 ,
                                "b1" = 0.3971,
                                "b2" = 0.3971,
                                "b3" = 0.0649,
                                "theta" = 0
                              ),
                              nbr_cores = 1,
                              squared_frame = NULL){
  object <- sf::st_sf(geometry=object)
  return( brk_dispersal(object,
                        size_raster,
                        tolerance_square,
                        kernel,
                        kernel.options,
                        nbr_cores,
                        squared_frame) )
}


#' @import fftwtools
#' @import parallel
#' @importFrom parallel makeCluster
#' @importFrom raster raster as.matrix
#' @importFrom fasterize fasterize
#' 
#' @export
brk_dispersal.sf <- function(object,
                             size_raster = 2^10,
                             tolerance_square = 0.1,
                             kernel = "NIG",
                             kernel.options = list(
                               "a1" = 0.2073 ,
                               "a2" = 0.2073 ,
                               "b1" = 0.3971,
                               "b2" = 0.3971,
                               "b3" = 0.0649,
                               "theta" = 0
                             ),
                             nbr_cores = 1,
                             squared_frame = NULL){
  
  message("Starting dispersal computing...")
  
  # --- message about raster size
  message("Raster size : ", format(size_raster, scientific = TRUE))
  if (size_raster >= 2^13){
    cat("WARNING : raster_size may be too large for memory usage RAM")
  }

  # --- warning if note square landscape
  

  if(is.null(squared_frame)){ squared_frame <- object  }
  bbox <- sf::st_bbox(squared_frame)
  
  if (!is_square_sf(squared_frame, tolerance = tolerance_square)) { #
    message_square <- paste("Landscape bbox isn't square, with tolerance", tolerance_square*100, "%!\n",
                            "Kernel dispertion may need squared rasterization cells\n")
    warning(message_square)
    print(bbox)
  }
  
  # ---------- 1. RASTERIZE
  #
  message("Step 1/2: Rasterize... ", appendLF = F)
  raster_sf <- raster::raster(squared_frame,
                              ncol = size_raster,
                              nrow = size_raster )
  # set specific patch_id!
  object$patch_id <- as.numeric(1:nrow(object))
  # 
  # As long as fasterize library does not fasterize for POITNS, we use the rasterize function of the raster library:
  if("sfc_POINT" %in% class(sf::st_geometry(object))){
    object_rasterized <- raster::rasterize(object, raster_sf, field = "patch_id")
  } else{
    object_rasterized <- fasterize::fasterize(object, raster_sf, field = "patch_id")
  }
  rm(raster_sf)
  message("done")
  
  # ---------------------------------------------
  
  # ---------- 2. FLUX CONVOLUTION
  # flux.convol : fait le produit de convolution pour calculer les
  # flux (dispersion avec Emission de maïs=1) dans le domaine
  # calcul d'une matrice de convolution par parcelle OGM :
  size_domain <- round(bbox$xmax - bbox$xmin)+1
  
  if (size_domain %% 2 == 1) {
    size_domain <- size_domain + 1
  }
  x = y = seq(
    from = -(size_domain / 2),
    to = size_domain / 2,
    length.out = size_raster
  )
  # KERNEL
  z <- outer(x, y, kernel, kernel.options)
  kernel_outer <- z / trapz2d(z) #on normalise le noyau
  rm(x)
  rm(y)
  rm(z)
  
  object_matrix <- raster::as.matrix(object_rasterized)
  df_bbox <- data.frame(xmin=bbox$xmin, xmax=bbox$xmax, ymin=bbox$ymin, ymax=bbox$ymax, row.names = NULL)
  crs <- st_crs(object)$proj4string
  
  if(nbr_cores > 1){
    
    message("Step 2/2: Parallel compute field... ", appendLF = T)
    # --- parLapply
    # cluster_env <- new.env(parent = as.environment("package:spERA"))
    cluster_env <- new.env()
    
    cl <- makeCluster(nbr_cores)
    clusterExport(cl=cl,
                  varlist=c("flux_conv", "object",
                            "object_matrix", "convolution",
                            "fftw_r2c_2d", "fftw2d",
                            "kernel_outer", "size_raster",
                            "shift_fft", "crs", "df_bbox"),
                  envir = cluster_env)
    
      flux_convol <- parallel::parLapply(
        cl = cl,
        X = 1:nrow(object),
        fun = function(i) flux_conv(size_raster = size_raster,
                                    iteration = object$patch_id[i],
                                    kernel_outer = kernel_outer,
                                    sources_matrix = object_matrix,
                                    crs = crs,
                                    bbox = df_bbox))
    
    stopCluster(cl)
    rm(cluster_env)
    
  } else{
    
    message("Step 2/2: Compute field... ", appendLF = F)
    flux_convol <- lapply(X = 1:nrow(object),
                          FUN = function(i) flux_conv(size_raster = size_raster,
                                                      iteration = object$patch_id[i],
                                                      kernel_outer = kernel_outer,
                                                      sources_matrix = object_matrix,
                                                      crs = crs,
                                                      bbox = df_bbox))
  }
  
  names(flux_convol) <- object$patch_id
  message("done", appendLF = TRUE)
  
  # --- object returned
  stackRaster_out <- raster::stack(flux_convol)
  names(stackRaster_out) <- paste0("subdomain.", 1:length(stackRaster_out@layers))
  attr(stackRaster_out, "time_created") <- Sys.time()

  return(stackRaster_out)
}


#' @importFrom sf st_as_sf
#' 
#' @export
#' 
brk_dispersal.SpatialPolygonsDataFrame <- function(object,
                             size_raster = 2^10,
                             tolerance_square = 0.1,
                             kernel = "NIG",
                             kernel.options = list(
                               "a1" = 0.2073 ,
                               "a2" = 0.2073 ,
                               "b1" = 0.3971,
                               "b2" = 0.3971,
                               "b3" = 0.0649,
                               "theta" = 0
                             ),
                             nbr_cores = 1,
                             squared_frame = NULL){
 
  sf_object <- sf::st_as_sf(object)
  return(
    brk_dispersal(object = sf_object,
                  size_raster,
                  tolerance_square,
                  kernel,
                  kernel.options,
                  nbr_cores,
                  squared_frame)
    
    
  )
}

############## PRIVATE ####################
#
# global convol
#
flux_conv <- function(size_raster,
                      iteration,
                      kernel_outer,
                      sources_matrix,
                      crs, bbox){
  
  message(iteration, "... ", appendLF = F)
  flush.console()
  
  sources_matrix[!sources_matrix %in% as.numeric(iteration)] <- 0
  sources_matrix[sources_matrix %in% as.numeric(iteration)] <- 1
  
  Fl_convol <- convolution(kernel_outer, sources_matrix, size_raster)
  
  raster_convol <-  raster::raster( Fl_convol,
                                    crs = crs,
                                    xmn = bbox$xmin,
                                    xmx = bbox$xmax,
                                    ymn = bbox$ymin,
                                    ymx = bbox$ymax)
  rm(Fl_convol)
  return(raster_convol)
}

############## PRIVATE ####################

#-------------------------------------
## convolution : convolution with FFT
# kernel: noyau, emission: répartition de la masse,s taille de la matrice (cf package fftwtools)

convolution <- function(kernel, emission, s) {
  fp = fftw_r2c_2d(kernel)
  fz = fftw_r2c_2d(emission)
  f1 = fz * fp
  p1 = fftw2d(f1, inverse = 1) #inverse du produit
  p <- Re(p1) / (s * s) #on prend la partie réelle et on re-normalise par la taille de la matrice
  # --- Le shift en C++!
  p <- shift_fft(p)
  return (p)
}

# remet les données en forme pour la convolution en 2D via FFT
shift_fft <- function(m) {
  i <- dim(m)[1]
  j <- dim(m)[2]
  n <- matrix(rep(0, i * j), nrow = i, ncol = j)
  i21 <- i / 2 + 1
  j21 <- j / 2 + 1
  i2 <- i / 2
  j2 <- j / 2
  
  n <- m[c(i21:i, 1:i2), c(j21:j, 1:j2)]
  
  return(n)
}

# Trapz2D, 2D quadrature with trapeze method
trapz2d <- function(p) {
  res = sum(apply(p, 2, trapz))
  return(res)
}


### Function copy/paste from pracma package
trapz <- function(x, y) {
  if (missing(y)) {
    if (length(x) == 0) return(0)
    y <- x
    x <- seq(along=x)
  }
  if (length(x) == 0 && length(y) == 0) return(0)
  if (!(is.numeric(x) || is.complex(x)) ||
      !(is.numeric(y) || is.complex(y)) )
    stop("Arguments 'x' and 'y' must be real or complex vectors.")
  m <- length(x)
  if (length(y) != m)
    stop("Arguments 'x', 'y' must be vectors of the same length.")
  if (m <= 1) return(0.0)
  
  xp <- c(x, x[m:1])
  yp <- c(numeric(m), y[m:1])
  n <- 2*m
  p1 <- sum(xp[1:(n-1)]*yp[2:n]) + xp[n]*yp[1]
  p2 <- sum(xp[2:n]*yp[1:(n-1)]) + xp[1]*yp[n]
  
  return(0.5*(p1-p2))
}

