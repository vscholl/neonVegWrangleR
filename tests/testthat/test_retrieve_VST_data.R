test_that("check if the package retrieves field data as data.frame", {
  tst <- retrieve_VST_data(site = "OSBS", start = 2019)
  expect_type(tst, list)

})
