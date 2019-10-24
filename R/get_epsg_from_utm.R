#' convert utmZone into corresponding EPSG
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples
#' @importFrom magrittr "%>%"
#'
get_epsg_from_utm <- function(utm){
  utm <-  substr(utm,1,nchar(utm)-1)
  if(as.numeric(utm)<10)utm <- paste('0', utm, sep="")
  epsg <- paste("326", utm, sep="")
  return(epsg)
}
