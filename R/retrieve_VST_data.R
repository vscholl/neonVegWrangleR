#' retrieve vegetation structure data from NEON
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @seealso [neonUtilities::loadByProduct()] which this function wraps.
#' @export
#' @examples
#' @importFrom magrittr "%>%"
#'
retrieve_VST_data <- function(site = "all", start = NA, enddate = NA, method = "shp"){
  
  # load NEON woody vegetation structure data product into R
  vst <- neonUtilities::loadByProduct("DP1.10098.001", check.size=F,
                                      site=site, start, enddate)
  
  # calculate UTM coordinates of vst entries based on azimuth and distance
  # measurements from plot reference points
  if(method == "geoneon"){
    vst <- calc_tree_geolocations(vst, dataProd = "vst_mappingandtagging")
  }else if(method == "shp"){
    vst$vst_mappingandtagging <- retrieve_coords_itc(vst$vst_mappingandtagging)
    #get latitude and longitude of all points
    vst$vst_mappping_latlon <- get_lat_long(vst$vst_mappingandtagging)
  }else if(method == "all"){
    vst2 <- calc_tree_geolocations(vst, dataProd = "vst_mappingandtagging")
    vst$vst_mappingandtagging <- retrieve_coords_itc(vst$vst_mappingandtagging)
    #get latitude and longitude of all points
    vst$vst_mappping_latlon <- get_lat_long(vst$vst_mappingandtagging)
    colnames(vst$vst_mappping_latlon)
    vst2_coords = vst2$vst_mappingandtagging %>% select(individualID, northing, easting, adjDecimalLatitude, adjDecimalLongitude)
    colnames(vst2_coords)[2:3]<- c("adjNorthing", "adjEasting")
    vst$vst_mappping_latlon = left_join(vst$vst_mappping_latlon, vst2_coords)
    vst$vst_mappping_latlon = unique(vst$vst_mappping_latlon)
  }
  
  attributes = vst$vst_apparentindividual %>%
    select(individualID, eventID, tempShrubStemID, tagStatus, growthForm, plantStatus, stemDiameter, 
           measurementHeight, height,baseCrownHeight, breakHeight,                 
           breakDiameter, maxCrownDiameter, ninetyCrownDiameter,          
           canopyPosition, shape, basalStemDiameter,         
           basalStemDiameterMsrmntHeight, maxBaseCrownDiameter, ninetyBaseCrownDiameter)
  colnames(vst$vst_mappping_latlon)[4] = "tagEventID"
  csv_vst = left_join(vst$vst_mappping_latlon, attributes) %>% unique
  colnames(csv_vst)[35:36] <- c("plotEasting", "plotNorthing")
  write_csv(csv_vst, "./outdir/field_data/vst_field_data.csv")

  return(vst)
}


