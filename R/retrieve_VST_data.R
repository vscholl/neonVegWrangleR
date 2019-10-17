#' retrieve vegetation structure data  from NEON
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @seealso [neonUtilities::loadByProduct()] which this function wraps.
#' @export
#' @examples
#' retrieve_VST_data("OSBS")
#' retrieve_VST_data("all", 2017, 2019)
#'
#'
retrieve_VST_data <- function(site = "all", start, enddate){
  #load vegetation structure in R
  vst <- neonUtilities::loadByProduct("DP1.10098.001", check.size=F, site=site, start, enddate)

  #calculate coordinates od vst entries
  vst_crd <- retrieve_coords_itc(vst$vst_mappingandtagging)
  return(vst)
}
