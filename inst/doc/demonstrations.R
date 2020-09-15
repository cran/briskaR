## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(fig.height = 6,
                      fig.width = 6,
                      fig.align = "center")

## ----loadPackages, echo=FALSE, message=FALSE----------------------------------
# 1. Load the main package
library("briskaR")
## Set briskaR working intern projection to LAMBERT 93 (default)
# briskaRSetInternProjection(LAMBERT_93)

# 2. Load 'ggplot2' for plotting objects. Not include in the package
# 3. load 'sf' for extra function on shapefiles
# 4. load 'raster' for extra function on raster
library("ggplot2")
library("sf")
library("raster")
library("sp") # plotting require this package
library("dplyr")

## ----loadLandscape------------------------------------------------------------
data("sfMaize65")

# PLOT ------------------------------------------------------------------------
ggplot() + theme_minimal() +
  scale_fill_manual(values = c("grey", "orange"),
                    name = "Maize") +
  geom_sf(data = sfMaize65,
          aes(fill = as.factor(maize)))

## ---- echo=FALSE--------------------------------------------------------------
sfMaize65$maize_GM<-sfMaize65$maize*c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0)

## ----select_GM, eval=FALSE----------------------------------------------------
#  # Random 20% are GM field
#  sfMaize65$maize_GM <- sfMaize65$maize * rbinom(n = nrow(sfMaize65), size = 1 , prob = 0.2)

## ----add_GM-------------------------------------------------------------------
# filter table to have only GM fields
sfMaize65_GM <- sfMaize65[sfMaize65$maize_GM == 1,]

## PLOT ------------------------------------------------------------------------ 
plt_GM <- ggplot() + theme_minimal() +
  scale_fill_manual(values = c("grey", "red"),
                    name = "GM maize") +
  geom_sf(data = sfMaize65,
          aes(fill = as.factor(maize_GM))) 
plt_GM +
  geom_sf_text(data = sfMaize65_GM,
                aes(label = label))

## ----addBuffer----------------------------------------------------------------
squareFrame_sfMaize65 <- st_squared_geometry(list(sfMaize65), buffer = 200) 

# PLOT ------------------------------------------------------------------------
plt_GM +
  geom_sf(data = squareFrame_sfMaize65, fill = NA)

## ----dispersal, cache=FALSE, warning=FALSE------------------------------------
stack_dispersal <- brk_dispersal(sfMaize65_GM,
                                 size_raster = 2^8,
                                 kernel = "geometric",
                                 kernel.options = list("a" =  -2.63),
                                 squared_frame = squareFrame_sfMaize65)

# PLOT ------------------------------------------------------------------------
raster::plot(stack_dispersal[[1:6]])

## ----toBEremoved, echo = FALSE------------------------------------------------
brk_emission <- function(sf, # geometry on which we applied the pollen emission pattern
                        keyTime,
                        key, # name of the column which is going to be created
                        FUN # a function of timeline?
    ){
      if("stackTimeline" %in% colnames(sf)) {
        stop("Please rename column 'stackTimeline' in sf object.")
      }
      # CREATE NEW COLUMN
      sf[["key_temp"]] <- lapply(1:nrow(sf), FUN)
      # check length with timeline
      if(all(sapply(sf[[keyTime]], length) != sapply(sf[["key_temp"]], length))){
        stop(paste0("element within list returned by FUN' has not the same length as list element of '", keyTime, "' column"))
      }
      # ADD StackTimeLine
      stackTimeline = sort(unique(do.call("c", sf[[keyTime]])))
      
      sf[[key]] = lapply(1:nrow(sf), function(i){
        index_matching = match(sf[[keyTime]][[i]], stackTimeline)
        res = rep(0,length(stackTimeline))
        res[index_matching] = sf[["key_temp"]][[i]]
        return(res)
      })
      # /!\ REPLACE timeline
      sf[[keyTime]] = lapply(1:nrow(sf), function(i) stackTimeline)
      warning(paste("The column variable", keyTime, "may have changed"))
      # remove "key_temp"
      sf[["key_temp"]] = NULL
  
  return(sf)
}

## ----addTimeline--------------------------------------------------------------
sfMaize65_GM_Pollen <- brk_timeline(sf = sfMaize65_GM,
                                     key = "timeline",
                                     from = as.Date("01-07-2018", format = "%d-%m-%y"),
                                     to = as.Date("01-09-2018", format = "%d-%m-%y"),
                                     by = "days")

## ----addPollenEmission--------------------------------------------------------
# Profile of emission:
data("maize.proportion_pollen")
graphics::plot(maize.proportion_pollen, type= "l")

funTimePollen <- function(time){
  density = runif(1, 7, 11)
  pollen = rgamma(1, shape = 1.6, scale = 1 / (2 * 10 ^ -7))
  nbr_days = length(time)
  deb = sample(1:(nbr_days - length(maize.proportion_pollen)), 1)
  end = (deb + length(maize.proportion_pollen) - 1)
  pollen_emission <- rep(0, nbr_days)
  pollen_emission[deb:end] <- as.numeric(pollen * density * maize.proportion_pollen)
  return(pollen_emission)
}

# ADD Column EMISSION To 
sfMaize65_GM_Pollen <- brk_emission(sf = sfMaize65_GM_Pollen,
                                    keyTime = "timeline", # Length of the reference timeline
                                    key = "EMISSION", # Name of the new column
                                    # function over each line of sf
                                    FUN = function(i){
                                          funTimePollen(sfMaize65_GM_Pollen$timeline[[i]])
                                    })

## ----exposure, cache=FALSE----------------------------------------------------
stackTimeline = seq(from = min(do.call("c", sfMaize65_GM_Pollen[["timeline"]])),
                    to = max(do.call("c", sfMaize65_GM_Pollen[["timeline"]])),
                    by = "days")

stack_exposure <- brk_exposure(stack_dispersal,
                               sfMaize65_GM_Pollen,
                               key = "EMISSION", # Name of the new column
                               keyTime = "timeline", # Length of the reference timeline
                               loss = 0.1,
                               beta = 0.2,
                               quiet = TRUE)
# PLOT ------------------------------------------------------------------------
raster::plot(stack_exposure[[1:6]])

## ----plot_outGM, error=TRUE---------------------------------------------------
# A MULTIPOLYGON OUT OF GM FIELDS
sfMaize65_outGM <- st_sf(geometry = st_difference(x = st_geometry(squareFrame_sfMaize65),
                                                  y = st_union(st_geometry(sfMaize65_GM))))

# POINTS OUT OF GM FIELDS
gridPOINT_squareFrame <- st_make_grid(squareFrame_sfMaize65, n = 30, what = "centers") # grid n x n !!
gridPOINT_outGM <- st_sf(geometry = st_intersection(x = st_geometry(gridPOINT_squareFrame),
                                                    y = st_union(st_geometry(sfMaize65_outGM))))

# PLOT ------------------------------------------------------------------------
ggplot() + theme_minimal() +
  geom_sf(data = sfMaize65_outGM, fill = "grey30") +
  geom_sf(data = gridPOINT_outGM) # point out of GM field

## ----plotExposure_outGM, error=TRUE-------------------------------------------
# Create raster
df_outGM = as.data.frame(raster::extract(x = stack_exposure,
                                         y =  sf::as_Spatial(gridPOINT_outGM)))
#colnames(df_outGM) = paste0("Time_", gsub("-", "\\1", stackTimeline))

sf_outGM = st_as_sf(geometry = st_geometry(gridPOINT_outGM), df_outGM)

# PLOT ------------------------------------------------------------------------
ggplot() + theme_minimal() +
  labs(title = paste("Exposure at", colnames(df_outGM)[40])) +
  scale_color_continuous(low = "green", high = "red",
                         trans = "log", name = "log scaled") +
  geom_sf(data = sf_outGM,
          aes(color = df_outGM[,40]))

## ----defineReceptor-----------------------------------------------------------
# Margins around each fields
sfMaize65_receptor = st_multibuffer(sfMaize65_GM,
                                    dist = rep(100, nrow(sfMaize65_GM)))

# PLOT ------------------------------------------------------------------------
plt_GMreceptor <-
  ggplot() + theme_minimal() +
    geom_sf(data = sfMaize65_GM,
            fill = "red") +
    geom_sf(data = sfMaize65_receptor,
            fill = "#669900")
plt_GMreceptor

## ----newInd-------------------------------------------------------------------
# 1. Number of site for the first generation
nbrSite = 100
# 2. Set eggs
sfLarvae <- brk_newPoints(sf = sfMaize65_receptor, size = nbrSite) # size = number of sites

# PLOT ------------------------------------------------------------------------
plt_GMreceptor +
  geom_sf(data = sfLarvae)

## ----lifeInd------------------------------------------------------------------
DateEmergence = sample(seq(as.Date("01-07-2018", format = "%d-%m-%y"),
                           as.Date("01-09-2018", format = "%d-%m-%y"), by = "days"),
                      size = nbrSite,
                      replace = TRUE)

sfLarvae = brk_timeline( sf = sfLarvae,
                         key = "Date",
                         from = DateEmergence,
                         to  =  DateEmergence + 20,
                         by  = "days")

## ----matchDate----------------------------------------------------------------
stackTimelineEMISSION = sort(unique(do.call("c", sfMaize65_GM_Pollen[["timeline"]])))

## ----computeExposure, warning=FALSE-------------------------------------------
exposureINDIVIDUAL <- brk_exposureMatch(stackRaster_exposure = stack_exposure,
                                        sf = sfLarvae,
                                        stackTimeline = stackTimelineEMISSION,
                                        keyTime = "Date",
                                        key = "EXPOSURE")

## ----computeDamage------------------------------------------------------------
damageLethal = function(x,LC50, slope){
      return(1/(1+(x/LC50)^slope))
}

LC50DR = 451*10^4
slopeDR = -2.63

damageINDIVIDUAL <- exposureINDIVIDUAL %>% 
  dplyr::mutate(DAMAGE = lapply(EXPOSURE, function(expos){damageLethal(expos, LC50DR, slopeDR)}))

## ----plotDamage---------------------------------------------------------------
DFdamage = data.frame(
  DAMAGE = do.call("c", damageINDIVIDUAL[["DAMAGE"]]),
  Date = do.call("c", damageINDIVIDUAL[["Date"]]) ) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(mean_DAMAGE = mean(DAMAGE, na.rm = TRUE),
                       q025_DAMAGE = quantile(DAMAGE, probs = 0.025, na.rm = TRUE),
                       q975_DAMAGE = quantile(DAMAGE, probs = 0.975, na.rm = TRUE),
                       min_DAMAGE = min(DAMAGE, na.rm = TRUE),
                       max_DAMAGE = max(DAMAGE, na.rm = TRUE))

minDateDAMAGE = data.frame(Date = do.call("c", damageINDIVIDUAL[["Date"]]))

ggplot() +
  theme_minimal() +
  labs(x = "Time", y = "Probability Distribution of Damage") +
  geom_line(data = DFdamage,
                 aes(x = Date, y = mean_DAMAGE), color = "red") +
  geom_ribbon(data = DFdamage,
              aes(x = Date, ymin = q025_DAMAGE, ymax = q975_DAMAGE), alpha = 0.5, color = NA, fill = "grey10") +
  geom_ribbon(data = DFdamage,
              aes(x = Date, ymin = min_DAMAGE, ymax = max_DAMAGE), alpha = 0.5, color = NA, fill = "grey90") 

