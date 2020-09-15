context("Test kernel implementation in Cpp")

# # ------------
# # data("sf_maize_65")
# 
# size_domain <- 4998.525 #st_bbox(sf_maize_65)$xmax - st_bbox(sf_maize_65)$xmin
# size_raster <- 2^10
# x = y = seq(
#   from = -(size_domain / 2),
#   to =  size_domain/ 2,
#   length.out = size_raster
# )
# 
# # --- test NIG
# test_that("test kernel NIG", {
#   
#   kernel.options <- list(
#     "a1" = 0.2073 ,
#     "a2" = 0.2073 ,
#     "b1" = 0.3971,
#     "b2" = 0.3971,
#     "b3" = 0.0649,
#     "theta" = 0
#   )
#   
#   outer_NIG_Cpp <- outer(x, y, "NIG_Cpp", kernel.options)
#   outer_NIG <- outer(x, y, "NIG", kernel.options)
#   
#   # all.equal(outer_NIG_Cpp, outer_NIG)
#   
#   expect_equal(outer_NIG_Cpp, outer_NIG)
# })
# 
# # --- test Geometric
# test_that("test kernel Geometric", {
#   
#   kernel.options <- list( "a" = -2.59 )
#   
#   outer_geometric_Cpp <- outer(x, y, "geometric_Cpp", kernel.options)
#   outer_geometric <- outer(x, y, "geometric", kernel.options)
# 
#   expect_equal(outer_geometric_Cpp, outer_geometric)
# })
# 
# # --- test FatTail
# test_that("test kernel FatTail", {
#   
#   outer_FatTail_Cpp <- outer(x, y, "FatTail_Cpp")
#   outer_FatTail <- outer(x, y, "FatTail")
#   
#   expect_equal(outer_FatTail_Cpp, outer_FatTail)
# })
# 
# # --- test kernel_fat_tail
# test_that("test kernel FatTail generic", {
#   
#   outer_FatTail_generic_Cpp <- outer(x, y, "kernel_fat_tail_Cpp")
#   outer_FatTail_generic <- outer(x, y, "kernel_fat_tail")
#   
#   expect_equal(outer_FatTail_generic_Cpp, outer_FatTail_generic)
# })
# 
# # --- test Student
# test_that("test kernel Student", {
#   
#   kernel.options = list(
#     "c1" = 1.12,
#     "a" = 1.55,
#     "b" = 1.45,
#     "c2" = 0,
#     "theta" = 0)
#   
#   outer_Student_Cpp <- outer(x, y, "student_Cpp", kernel.options)
#   outer_Student <- outer(x, y, "student", kernel.options)
#   
#   expect_equal(outer_Student_Cpp, outer_Student)
# })

