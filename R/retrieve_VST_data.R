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
retrieve_VST_data <- function(site = "all", start = NA, enddate = NA){
  
  # load NEON woody vegetation structure data product into R
  vst <- neonUtilities::loadByProduct("DP1.10098.001", check.size=F,
                                      site=site, start, enddate)
  
  # calculate UTM coordinates of vst entries based on azimuth and distance
  # measurements from plot reference points
  vst <- calc_tree_geolocations(vst, dataProd = "vst_mappingandtagging")

  return(vst)
}


