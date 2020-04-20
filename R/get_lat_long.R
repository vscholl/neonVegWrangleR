#' calculate latitude and longitude values for each stem in the vst data
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
    colnames(dat)[31:32] = c("plotLatitude", "plotLongitude")
    # dat <- dplyr::select(dat, c("individualID", "eventID",
    #                            "domainID", "siteID", "plotID", "taxonID",
    #                            "scientificName", "utmZone",
    #                            "Northing","Easting", "elevation"))
    #dat <- dat[complete.cases(dat),]
    #todo: check the UTMS
    epsg <- get_epsg_from_utm(as.character(unique(dat$utmZone)))
    # transform coordinates to lat/lon and add these columns to the df
    for(ii in 1:length(epsg)){
      utmZn <- substr(epsg, 4, nchar(epsg))
      utmZn <- paste(as.integer(utmZn), "N", sep = "")
      dt = dplyr::filter(dat, utmZone == utmZn[ii])
      utm_coords <- dt[c("Easting", "Northing")]
      sp::coordinates(dt) <- c("Easting", "Northing")
      sp::proj4string(dt) <- sp::CRS(paste("+init=epsg:", epsg[ii], sep = ""))
      CRS.new <- sp::CRS("+init=epsg:4326")
      dt <- sp::spTransform(dt, CRS.new)
      coords_dat <- dt@coords
      colnames(dt@coords) <- c("itcLongitude", "itcLatitude")
      new_dat <- rbind(new_dat, cbind(dt@data, utm_coords, dt@coords))
    }
  }
  return(new_dat)
}
