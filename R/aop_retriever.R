#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples from_inventory_to_shp()
#' @importFrom magrittr "%>%"
#' @import neonUtilities, tidyverse, readr
#'
aop_retriever <- function(data, years,
                          products=c("DP3.30024.001","DP3.30006.001",
                                     "DP3.30025.001", "DP3.30025.001",
                                     "DP3.30015.001", "DP1.30003.001")){
  #extract information needed to get AOP tiles
  coords_for_tiles <- data %>%
    dplyr::select(plotID, siteID, api.utmZone, easting, northing)
  #get tiles dimensions
  coords_for_tiles$easting <- as.integer(coords_for_tiles$easting / 1000) * 1000
  coords_for_tiles$northing <- as.integer(coords_for_tiles$northing / 1000) * 1000

  #get list of tiles with vegetation structure
  tiles <- coords_for_tiles[-1] %>% unique
  tiles <- tiles[complete.cases(tiles),]
  #convert CHEQ into STEI (only the latter on the portal)
  tiles[tiles$siteID == "CHEQ", "siteID"] <- "STEI"
  tiles %>% unique
  #loop through tiles and data products: default is topographic and RS data
  for(ii in 1:nrow(tiles)){
    for(prd in products){
      #elevation
      byTileAOP(prd, site = tiles[ii,"siteID"],
                # year = years[years$siteID %in% tiles[ii, "siteID"], "scanDate"],
                year = years,tiles[ii,"easting"], tiles[ii,"northing"],
                buffer = 0, check.size = F, savepath = paste("./outdir/", prd,"/", sep=""))
    }
  }
}
