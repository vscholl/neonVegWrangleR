test_that("check if the package retrieves field data as data.frame", {
  # retrieve data for the OSBS site for 2019
  tst <- retrieve_VST_data(site = "OSBS", start = 2019)
  # check that the data retrieved is a data frame
  expect_is(tst,'list')

})
