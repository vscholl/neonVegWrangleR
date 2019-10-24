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
  new_dat <- NULL
  for(NeonSites in unique(field_data$siteID)){
    dat <- field_data[field_data$siteID %in% NeonSites,]
    dat <- dplyr::select(dat, c("individualID", "eventID",
                               "domainID", "siteID", "plotID", "taxonID",
                               "scientificName", "api.utmZone",
                               "northing","easting", "api.elevation"))
    dat <- dat[complete.cases(dat),]
    #todo: check the UTMS
    epsg <- get_epsg_from_utm(unique(dat$api.utmZone))
    for(ii in 1:length(epsg)){
      utmZn <- substr(epsg,4,nchar(epsg))
      utmZn <- paste(as.integer(utmZn), "N", sep="")
      dt = dplyr::filter(dat, api.utmZone==utmZn[ii])
      utm_coords <- dt[c("easting", "northing")]
      coordinates(dt) <- c("easting", "northing")
      proj4string(dt) <- CRS(paste("+init=epsg:", epsg[ii], sep =""))
      CRS.new <- CRS("+init=epsg:4326")
      dt <- spTransform(dt, CRS.new)
      coords_dat <- dt@coords
      colnames(dt@coords) <- c("latitude", "longitude")
      new_dat <- rbind(new_dat, cbind(dt@data, utm_coords, dt@coords))
    }
  }
  return(new_dat)
}
