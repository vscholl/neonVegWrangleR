#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples from_inventory_to_shp()
#' @importFrom magrittr "%>%"
#' @import sf
#'
from_inventory_to_shp <- function(stem_locations=NULL
              #,veg_types = c("small tree", "single bole tree", "multi-bole tree")
              ,outdir= "./outdir/field_data/neon_vegetation.shp"){
  
  # get lat/lon for all trees in the dataset
  stem_locations_shp = get_lat_long(stem_locations)
  
  # transform into spatial data
  stem_locations_shp <- sf::st_as_sf(stem_locations_shp,
              coords = c("latitude", "longitude"), crs = 4326)

  # write to shapefile
  sf::st_write(stem_locations_shp, outdir, delete_layer=TRUE)
  
  return(stem_locations_shp)

}
