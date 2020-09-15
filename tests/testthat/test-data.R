# data(maize_65)
# context("Data format")
# 
# test_that("maize_65 is Landscape Format", {
#   expect_match(class(maize_65)[1],"Landscape")
#   expect_equal(maize_65@n,460)
#   expect_true(all(lapply(maize_65@thelandscape@polygons,slot,"ID") == row.names(maize_65@thelandscape)))
#   expect_true(all(lapply(maize_65@thelandscape@polygons,slot,"ID") == row.names(maize_65@thelandscape@data)))
#   expect_true(all(maize_65@thelandscape@data[1,]  == maize_65@thelandscape@data["0",]))
# })
# 
# test_that("maize Landscape datas Format", {
#   expect_match(class(maize.landscape)[1],"Landscape")
#   expect_equal(maize.landscape@n,460)
#   expect_true(all(lapply(maize.landscape@thelandscape@polygons,slot,"ID") == row.names(maize.landscape@thelandscape)))
#   expect_true(all(lapply(maize.landscape@thelandscape@polygons,slot,"ID") == row.names(maize.landscape@thelandscape@data)))
#   expect_true(all(maize.landscape@thelandscape@data[1,] == maize.landscape@thelandscape@data["0",]))
#   expect_true(row.names(maize.landscape@thelandscape@data[1,]) == row.names(maize.landscape@thelandscape@data["0",]))
# })
# 
# test_that("maize emitted pollen data", {
#   expect_equal(nrow(maize.emitted_pollen),nrow(maize.landscape@thelandscape[maize.landscape@thelandscape$sources==1,]))
#   expect_equal(row.names(maize.emitted_pollen),row.names(maize.landscape@thelandscape[maize.landscape@thelandscape$sources==1,]))
# })
# 
# test_that("maize Individuals data", {
#   expect_match(class(maize.individuals)[1],"Individuals")
# })
# 
# rm(list=ls())
