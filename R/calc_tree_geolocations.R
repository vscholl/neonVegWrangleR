#' calculate NEON woody vegetation structure individual geolocations.
#' more about the geoNEON package here:
#' https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON
#'
#' @return A list of dataframe
#' @seealso [geoNEON::def.calc.geo.os()] which this function wraps
#' @export
#' @examples
#' calc_tree_geolocations(data = vst
#'                        , dataProd = "vst_mappingandtagging")
#'
calc_tree_geolocations <- function(data = vst
                                   , dataProd = "vst_mappingandtagging"){

  # using the NEON function, calculate precise geolocations for each
  # mapped individual, from distance & azimuth to easting & northing.
  # must specify "vst_mappingandtagging" as the data product.
  vst_loc <- def.calc.geo.os(data = data[[dataProd]]
                              , dataProd = dataProd) %>%

    # Rename the adjEasting and adjNorthing columns
    dplyr::rename(easting = adjEasting
                  , northing = adjNorthing)
  data[[dataProd]] <- vst_loc
  return(vst)
}
