#' get individualID coordinates
#'
#'function to calculate the coordinates for each
#'individual tree in the vegetation structure survay
#' @inheritParams str_detect
#' @return a dataframe
#' @seealso [geoNEON::loadByProduct()] which this function wraps.
#' @export
#' @examples
#' retrieve_VST_data("OSBS")
#' retrieve_VST_data("all", 2017, 2019)
#'
#'
retrieve_coords_itc <- function(dat){

  #import shapefile with the coordinates of plots and pointIDs for NEON vegetation structure
  plots<-st_read("./meta/NEON_TOS.shp") %>% data.frame %>%
    filter(str_detect(appMods,"vst"))

  # mutate  point and plot id into factors, and remove multiple entries
  dat<-dat %>%
    mutate(pointID=factor(pointID, levels = levels(unique(plots$pointID))) )%>%
    mutate(plotID=factor(plotID, levels = levels(unique(plots$plotID)))) %>%
    inner_join(plots,by=c("plotID","pointID"))

  # check if there are individualIDs missing teh azimuth, remove them in case
  if(sum(is.na(dat["stemAzimuth"]))>0){
    warning(paste(sum(is.na(dat["stemAzimuth"])),
    "entries could not be georeferenced
    and will be discarded."))
    dat <- dat[!is.na(dat["stemAzimuth"]), ]
  }

  #calculate UTM coordinates for individualIDs
  dat_apply <- dat %>%
    dplyr::select(c(stemDistance, stemAzimuth, easting, northing))
  coords <- apply(dat_apply,1,
    function(p)retrieve_dist_to_utm(p[1],p[2], p[3], p[4])) %>%
    t %>%
    data.frame
  colnames(coords) <- c('UTM_E', 'UTM_N')
  field_tag <- cbind(dat, coords) %>% filter(!is.na(UTM_E))
  return(field_tag)

}
