test_that("check if the stem coordintates match between geoNEON and manual calculations", {
  
  setwd("../..")
  
  # download NEON woody vegetation data for a specific site
  tst <- neonUtilities::loadByProduct("DP1.10098.001"
                                      , check.size = F
                                      , site = "NIWO"
                                      , startdate = 2016)
  
  # use geoNEON package to calculate UTM coordinates for each stem
  loc_geoNEON <- calc_tree_geolocations(data = tst)
  
  # use MANUAL calculation to calculate UTM coordinates for each stem
  loc_manual <- retrieve_coords_itc(data = tst)
  
    
  # check expected coordinate of tree with specific individual ID
  sample_geoNEON <- loc_geoNEON$vst_mappingandtagging %>% 
    dplyr::filter(individualID == "NEON.PLA.D13.NIWO.00267") %>% 
    dplyr::select(northing, easting) %>% as.numeric
  sample_manual <- loc_manual$vst_mappingandtagging %>% 
    dplyr::filter(individualID == "NEON.PLA.D13.NIWO.00267") %>% 
    dplyr::select(northing, easting) %>% as.numeric
  

    # check if the coordinates are the same between methods
    #expect_equal(tst_coord, c(4433563, 450101))
  
    # calculate distances between each pair of stem coordinates
    for(i in 1:nrow(loc_geoNEON$vst_mappingandtagging)) {
      
      print(i)
      print(loc_geoNEON$vst_mappingandtagging$individualID[i])
      print(loc_manual$vst_mappingandtagging$individualID[i])
      
      
      # geoNEON coordinates
      x1 <- loc_geoNEON$vst_mappingandtagging$easting[i]
      y1 <- loc_geoNEON$vst_mappingandtagging$northing[i]
      
      # manual calculations using trigonometry 
      x2 <- loc_manual$vst_mappingandtagging$UTM_E[i]
      y2 <- loc_manual$vst_mappingandtagging$UTM_N[i]
      
      dist <- sqrt(sum( ((x2 - x1) ^ 2) + (y2 - y1) ^ 2))
      
      if(i == 1){
        coord_difs <- dist
      } else{
        coord_difs <- rbind(coord_difs, dist)
      }
      
      
    }
  
   # calculate a histogram of differences between coordinates.
  # first, create a data frame showing the distance between each pair of 
  # stem coordinates, include the individual ID 
  coord_difs_df <- as.data.frame(cbind(loc_geoNEON$vst_mappingandtagging$individualID, 
                                       coord_difs)) 
  colnames(coord_difs_df) <- c("individualID", "distance")
  library(ggplot2)
  ggplot(coord_difs_df, aes(x=distance)) + 
    geom_histogram(binwidth=0.5)
   
   # write a shapefile to see how far the 
  
})