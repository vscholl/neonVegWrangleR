#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples
#' @importFrom magrittr "%>%"
#' @import rgdal
#'
get_lat_long <- function(field_data){
  library(rgdal)
  new_dat <- NULL
  for(NeonSites in unique(field_data$siteID)){
    dat <- field_data[field_data$siteID %in% NeonSites,]
    epsg <- get_epsg_from_utm(unique(dat$utmZone))
    dat <- dat[complete.cases(dat$Easting), ]
    utm_coords <- dat[c("Easting", "Northing")]
    coordinates(dat) <- c("Easting", "Northing")
    proj4string(dat) <- CRS(paste("+init=epsg:", epsg, sep =""))
    CRS.new <- CRS("+init=epsg:4326")
    dat <- spTransform(dat, CRS.new)
    coords_dat <- dat@coords
    colnames(dat@coords) <- c("latitude", "longitude")
    new_dat <- rbind(new_dat, cbind(dat@data, utm_coords, dat@coords))
  }
  return(new_dat)
}
