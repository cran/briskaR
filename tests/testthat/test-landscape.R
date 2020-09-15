# context("Landscape methods")
# 
# test_that("internal functions", {
# 
#   n = 500
#   prop = 0.4
#   range = 10
#   xmin = 0
#   xmax = 5000
#   ymin = 0
#   ymax = 5000
#   test_that(points <- briskaR:::create.voronoi.points(n, prop, range, xmin, xmax, ymin, ymax), succeed())
# 
#   test_that(map <- briskaR:::create.voronoi.diagram(layer = points, xmin, xmax, ymin, ymax), succeed())
# 
#   nb_fields=100
#   max_size=5000
#   nb_ind=100
#   test_that(objectLTemp <- briskaR:::simulateInitialPartition(nb_fields,0.4,10,0,max_size,0,max_size), succeed())
# 
#   border_size = 200
#   prob = runif(1, 0.1, 0.9)
#   mean_thickness = runif(1, 2, 20)
#   v_thickness = 50
#   test_that(objectL <- briskaR:::simulateThickMargins(objectLTemp, border_size, prob, mean_thickness, v_thickness), succeed())
# 
# })

# test_that("sf methods for landscape",{
#   
#   data("sf_maize_65")
#   sf_maize_65$exposure_type <- sample(c("s", "r", "n"), nrow(sf_maize_65), replace = TRUE)
#   sf_maize_65_sources <- sf_maize_65[sf_maize_65$exposure_type == "s", ][1:10, ]
#   
#   data("maize.proportion_pollen")
#   test_that(sf_pollen_emission <- create_pollen_sources(sf_maize_65_sources), succeed())
#   
#   # dispersal
#   test_that(stack_dispersal <- brk_dispersal(sf_maize_65_sources), succeed())
#   # exposure
#   test_that(brk_exposure(stack_dispersal, sf_pollen_emission), succeed())
#   # toxicIntensity
#   test_that(toxicIntensity(sf_maize_65_sources, sf_pollen_emission), succeed())
#   
# })

# test_that("load landscape", {
# 
#   data(maize_65)
#   maize_data <- maize_65@data
#   maize_sp_only <- maize_65 ; maize_sp_only@data = data.frame(remove = rep(0,nrow(maize_65@data)))
#   #
#   test_that(load_landscape <- loadLandscape(maize_sp_only, maize_data), succeed())
# 
# })

# test_that("save SIG", {
#   data(maize_65)
#
#   ti <- toxicIntensity(maize.landscape,maize.emitted_pollen)
#
#   saveIntoFile(maize.landscape, ti, filename = "ParticlesDispersion.tiff", format="GTiff", overwrite=TRUE)
#
# })




