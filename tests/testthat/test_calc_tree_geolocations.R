test_that("check if the coordintates are reliable", {
  setwd("../..")
  tst <- neonUtilities::loadByProduct("DP1.10098.001"
                                      , check.size = F
                                      , site = "NIWO"
                                      , startdate = 2016)
  tst_loc <- calc_tree_geolocations(data = tst)

  # check expected coordinate of tree with specific individual ID
  tst <- tst %>% dplyr::filter(individualID == "") %>%
    dplyr::select(northing, easting) %>%
    as.numeric %>%
    round()

  expect_equal(tst, c(3284946, 404040))

})
