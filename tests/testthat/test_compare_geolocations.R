test_that("check if the stem coordintates match between geoNEON and manual calculations", {
  
  setwd("../..")
  tst <- neonUtilities::loadByProduct("DP1.10098.001"
                                      , check.size = F
                                      , site = "NIWO"
                                      , startdate = 2016)
  tst_loc <- calc_tree_geolocations(data = tst)
  
  # check expected coordinate of tree with specific individual ID
  tst_coord <- tst_loc$vst_mappingandtagging %>% 
    dplyr::filter(individualID == "NEON.PLA.D13.NIWO.00267") %>%
    dplyr::select(northing, easting) %>%
    as.numeric %>%
    round()
  
  expect_equal(tst_coord, c(4433563, 450101))
  
})