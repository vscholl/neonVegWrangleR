test_that("pull VST data from NEON, geolocate, check that the coordinates are correct", {
  # retrieve data for the OSBS site for 2019
  tst <- retrieve_VST_data(site = "OSBS", start = 2019)
  #calculate coordinates od vst entries
  vst <- calc_tree_geolocations(vst$vst_mappingandtagging)
  # check that the data retrieved is a data frame
  expect_is(tst,'list')

})


