test_that("check if the shapefile is written to file", {
  setwd("../..")
  # pull veg structure data into R
  tst <- neonUtilities::loadByProduct("DP1.10098.001"
                                      , check.size = F
                                      , site = "NIWO"
                                      , startdate = 2016)
  # calculate UTM locations for each individual
  tst_loc <- calc_tree_geolocations(data = tst)
  # calculate geographic (lat,long) coordinates for each individual
  tst_utm <- tst_loc$vst_mappingandtagging 
  tst_geo <- get_lat_long(field_data = tst_utm)
  
  # write shapefile for each tree 
  out_dir <- "./outdir/field_data/neon_vegetation.shp"
  tst_shp <- from_inventory_to_shp(stem_locations = tst_geo,
                                   outdir = out_dir)
  
  # check that the shapefile was created correctly
  expect_equal(file.exists(out_dir), TRUE)
  
  
  
})