#' get individualID coordinates
#'
#'function to calculate the coordinates for each
#'individual tree in the vegetation structure survay
#' @return a dataframe
#' @seealso [geoNEON::loadByProduct()] which this function wraps.
#' @importFrom magrittr "%>%"
#' @import sf
#' @import  dplyr
#' @import stringr
#'
#' @examples
#' retrieve_VST_data("OSBS")
#' retrieve_VST_data("all", 2017, 2019)
#'
#'
retrieve_coords_itc <- function(dat){
  #import shapefile with the coordinates of plots and pointIDs for NEON vegetation structure
  plots<-sf::st_read("./meta/All_NEON_TOS_Plot_Points.shp") %>% data.frame %>%
    dplyr::filter(str_detect(appMods,"vst"))
  
  plots<-plots %>%
    mutate(pointID=as.character(pointID))%>%
    mutate(siteID=as.character(siteID))%>%
    mutate(plotID=as.character(plotID))
  
  # mutate  point and plot id into factors, and remove multiple entries
  dat<-dat %>%
    mutate(pointID=as.character(pointID))%>%
    mutate(siteID=as.character(siteID))%>%
    mutate(plotID=as.character(plotID)) %>%
    inner_join(plots,by=c("plotID","pointID", "siteID"))
  
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
  colnames(coords) <- c('itcEasting', 'itcNorthing')
  #colnames(dat)[54:55] <- c("plotEasting", "plotNorthing")
  #colnames(dat)[27:28] <- c("geoEasting", "geoNorthing")
  
  field_tag <- cbind(dat, coords) %>% filter(!is.na(itcEasting))
  return(field_tag)
  
}

