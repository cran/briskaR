# FUNCTIONAL DATA
#
#
#
## quiets concerns of R CMD check re: the .'s that appear in pipelines
# if(getRversion() >= "3.0.0")  utils::globalVariables(c(".", "st_fd<-"))

# @export
# brk_isFD <- function(x){ "brkFD" %in% attributes(x) }

# # @export
# get_fd.sf <- function(.sf){
#   sf_fd <- .sf %>% select_(which(sapply(., is_fd)))
#   sf_fd$geometry = NULL # while a geometry is also a functional data!
#   return(sf_fd)
# }


#' @name brk_addFD
#' 
#' @title Functional DATA
#'  
#' @description Add time series to sf objects
#' 
#' @param sf object of class \link[sf]{sf}. See \link[sf]{sf} for details.
#' @param key the name of the new column, as strings or symbols
#' @param FUN the function to be applied to each element of \code{sf}.
#'  In the case of functions like +, %*%, the function name must be
#'   backquoted or quoted. See lapply functions for details.
#' @param \dots optional arguments to FUN. See lapply functions for details.
#' 
#' @return A \link[sf]{sf} object with addition functional data feature (or feature dynamic).
#' 
#' @export
#' 
brk_addFD <- function(sf, key, FUN, ...){
    sf[[key]] <- lapply(1:nrow(sf), FUN, ...)
    # class(sf) <- c(class(.sf), "sffd")
    attr(sf, "brkFD") <- c(deparse(substitute(key)), attr(sf, "brkFD"))
  return(sf)
}

#' @name brk_addFD
#' 
#' @title Function used to add functional data in \link[sf]{sf} objects.
#' 
#' @param keyConstraint character string. The reference of the column to be checked
#' 
#' @export
#' 
brk_addFD2 <- function(sf, # geometry on which we applied the pollen emission pattern
                       keyConstraint,
                       key, # name of the column which is going to be created
                       FUN,
                       ...){
  if("stackTimeline" %in% colnames(sf)) {
    stop("Please rename column 'stackTimeline' in sf object.")
  }
  # CREATE NEW COLUMN
  sf[["key_temp"]] <- lapply(1:nrow(sf), FUN, ...)
  # check length with timeline
  if(all(sapply(sf[[keyConstraint]], length) != sapply(sf[["key_temp"]], length))){
    stop(paste0("element within list returned by FUN' has not the same length as list element of '", keyConstraint, "' column"))
  }
  # ADD StackTimeLine
  stackTimeline = sort(unique(do.call("c", sf[[keyConstraint]])))
  
  sf[[key]] = lapply(1:nrow(sf), function(i){
    index_matching = match(sf[[keyConstraint]][[i]], stackTimeline)
    res = rep(0,length(stackTimeline))
    res[index_matching] = sf[["key_temp"]][[i]]
    return(res)
  })
  # /!\ REPLACE timeline
  sf[[keyConstraint]] = lapply(1:nrow(sf), function(i) stackTimeline)
  warning(paste("The column variable", keyConstraint, "may have changed"))
  # remove "key_temp"
  sf[["key_temp"]] = NULL
  
  return(sf)
}


#' @title find index
#' 
#' @param sf An object of class \link[sf]{sf}
#' @param key character string. name of the column to select
#' @param value value of the element to return index from the column defined by key
#' 
#' @return vector if not all index are equal. scalar if all equal.
#'   
#' @export
#' 
brk_findIndexFD <- function(sf, key, value){
  returnID = sapply(1:nrow(sf), function(i){ match(value, sf[[key]][[i]])  })
  if(length(unique(returnID)) == 1){ returnID = unique(returnID) }
  return(returnID)
}

#' @name brk_cFilterFD 
#' 
#' @title Function used to filter functional data in \link[sf]{sf} objects.
#' 
#' @param sf sf. An object of class \link[sf]{sf}
#' @param key character string. The name of the column to select
#' @param index integer (or vector). The index of the functional data.
#' 
#' @export
#' 
brk_cFilterFD <- function(sf, key, index){
  sf[[key]] <- do.call("c", lapply(sf[[key]] , `[[`, index) )
  return(sf)
}

#' @name brk_cFilterFD 
#' 
#' @export
#' 

brk_cFilterFD_ <- function(sf, key, index){
  if(length(index) == 1){ index = rep(index, nrow(sf)) }
  if(length(index) != nrow(sf)){ stop("length of vector index is different of 1 and nrow(sf).")}
  sf[[key]] <- do.call("c", lapply(1:nrow(sf), function(i) sf[[key]][[i]][index[i]] ))
  return(sf)
}

#' @name brk_cFilterFD 
#' 
#' @param key1 character string. The name of the column to select
#' @param key2 character string. The name of the column to select
#' 
#' @export
#' 
brk_cFilterFD2 <- function(sf, key1, key2, index){
  sf[[key1]] <- do.call("c", lapply(sf[[key1]] , `[[`, index) )
  sf[[key2]] <- do.call("c", lapply(sf[[key2]] , `[[`, index) )
  return(sf)
}

#' @name brk_cFilterFD 
#' 
#' @param key3 character string. The name of the column to select
#' 
#' @export
#' 
brk_cFilterFD3 <- function(sf, key1, key2, key3, index){
  sf[[key1]] <- do.call("c", lapply(sf[[key1]] , `[[`, index) )
  sf[[key2]] <- do.call("c", lapply(sf[[key2]] , `[[`, index) )
  sf[[key3]] <- do.call("c", lapply(sf[[key3]] , `[[`, index) )
  return(sf)
}


#' @title Convert list.column data.frame into scalar.column data.frame
#' 
#' @description Convert data.frame with 2 column.list into data.frame with only column.scalar 
#' 
#' @name brk_FDtoDF 
#' 
#' @param sf sf. An object of class \link[sf]{sf}
#' @param key1 character string. The name of the column to select
#' @param key2 icharacter string. The name of the column to select
#' @param id name of the replicate for the id. As to be of the same length as the number of row of the sf object
#' 
#' @export
#' 
brk_FDtoDF_ <- function(sf, key1, key2, id = NULL){
  lgth_key1 = sapply(sf[[key1]], length)
  lgth_key2 = sapply(sf[[key2]], length)
  if(any(lgth_key1 != lgth_key2)){ stop("length of element from 'key1' and 'key2' differ.")}
  if(is.null(id)) {
    id = 1:length(lgth_key1)
  } else{
    id = sf[[id]]
  }
  df = data.frame(
    id = do.call("c", lapply(1:length(id), function(i) rep(id[i], lgth_key1[[i]])))
  )
  df[[key1]] = do.call("c", sf[[key1]])
  df[[key2]] = do.call("c", sf[[key2]])
  return(df)
}


#' @name brk_FDtoDF 
#' 
#' @param keep vector of column name to keep
#' 
#' @export
#' 
brk_FDtoDF <- function(sf, key1, key2, id = NULL, keep = NULL){
  
  DF = brk_FDtoDF_(sf, key1, key2, id)
  
  # --- keep
  if(!is.null(keep)) {
    for(i in 1:length(keep)){
      DF[[keep[[i]]]] = brk_FDtoDF_STICK(sf, key1, keep[[i]])
    }
  }
  return(DF)
}

#' @name brk_FDtoDF 
#'  
#' @export
#' 
brk_FDtoDF_STICK = function(sf, key1, keep){
  vec=do.call("c", lapply(1:nrow(sf),
                          function(i){ rep(sf[[keep]][i], length(sf[[key1]][i]))}))
  return(vec)
}  

#' @title Combine list of data.frame by Rows
#' 
#' @description Function used to filter functional data in \link[sf]{sf} objects.
#' 
#' @return Return a data.frame
#' 
#' @param ls A list of data.frame
#' @param id id to provide to each data.frame. Must be the length of the list 
#' 
#' @export
#' 
brk_rbindLStoDF <- function(ls, id = NULL){
  df <- do.call("rbind", ls)
  nrow_df <- sapply(ls, nrow)
  if(is.null(id)) { id = 1:length(nrow_df) }
  df[["id"]] = do.call("c", lapply(1:length(nrow_df), function(i) rep(id[i], nrow_df[[i]])))
  return(df)
}
