test_that("check if the coordintates are reliable", {
  setwd("../..")
  tst <- retrieve_VST_data(site = "OSBS", start = 2019)
  tst <- tst %>% dplyr::filter(individualID == "NEON.PLA.D03.OSBS.00116") %>%
    dplyr::select(UTM_N, UTM_E) %>%
    as.numeric %>%
    round()

  expect_equal(tst, c(3284946, 404040))

})
