test_that("check if the LAT/LONG coordintates are reliable", {
  setwd("../..")
  tst <- neonUtilities::loadByProduct("DP1.10098.001"
                                      , check.size = F
                                      , site = "NIWO"
                                      , startdate = 2016)
  tst_loc <- calc_tree_geolocations(data = tst)
  
  # check expected LAT/LONG coordinate of tree with specific individual ID
  tst_utm <- tst_loc$vst_mappingandtagging 
  tst_geo <- get_lat_long(field_data = tst_utm)
  tst_coord <- tst_geo %>% 
    dplyr::filter(individualID == "NEON.PLA.D13.NIWO.00267") %>%
    dplyr::select(latitude, longitude) %>%
    as.numeric %>%
    round()
    
  expect_equal(tst_coord, c(-106, 40))
  
})
