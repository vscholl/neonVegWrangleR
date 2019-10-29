#' download AOP data where vst data exists for specified year and site
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples from_inventory_to_shp()
#' @importFrom magrittr "%>%"
#' @import neonUtilities, tidyverse, readr
#'
retrieve_aop_data <- function(data, year,
                          products = c("DP3.30024.001"   # lidar-derived DTM, DSM
                                       ,"DP3.30006.001"  # hyperspectral reflectance
                                       ,"DP3.30025.001"  # lidar-derived slope, aspect
                                       ,"DP3.30015.001"  # canopy height model
                                       #,"DP1.30003.001" # lidar point cloud
                          )){

  # extract information needed to get AOP tiles
  coords_for_tiles <- data %>%
    dplyr::select(plotID, siteID, api.utmZone, easting, northing)
  # collect years per plot per date
  year = substr(year, 1, 4)
  coords_for_tiles <- cbind.data.frame(coords_for_tiles, year)

  # get tiles dimensions
  coords_for_tiles$easting <- as.integer(coords_for_tiles$easting / 1000) * 1000
  coords_for_tiles$northing <- as.integer(coords_for_tiles$northing / 1000) * 1000

  # get list of tiles with vegetation structure
  tiles <- coords_for_tiles[-1] %>% unique
  tiles <- tiles[complete.cases(tiles),]
  # convert CHEQ into STEI (only the latter on the portal)
  tiles[tiles$siteID == "CHEQ", "siteID"] <- "STEI"
  tiles <- tiles %>% unique
  # loop through tiles and data products: default is topographic and RS data
  for(ii in 1:nrow(tiles)){
    for(prd in products){
      tryCatch({
        #elevation
        neonUtilities::byTileAOP(prd,
                                 site = tiles[ii, "siteID"],
                                 year = tiles[ii,"year"],
                                 tiles[ii, "easting"],
                                 tiles[ii,"northing"],
                                 buffer = 0,
                                 check.size = F,
                                 savepath = paste("./outdir/",
                                                  prd,
                                                  "/",
                                                  sep = ""))

      }, error = function(e) {
        print(paste("site",tiles[ii,"siteID"],
                    "could not be fully downloaded! Error in retrieving:",
                    prd, "for year",  tiles[ii,"year"], ". error returned:", e))
      })
    }
  }
}
