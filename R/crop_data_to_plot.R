#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples apply(plots, 1, clip_plot, list_data, bff=bff)
#' @importFrom magrittr "%>%"
#' @import sf, reticulate
crop_data_to_plot <- function(dtprd= "DP3.30015.001", plots, bff=22){
  #get path of files in
  for(dataproduct in dtprd){
    list_data <- list.files(path = paste("./outdir/", dataproduct, sep=""), full.names = T,
                            pattern = ".tif|.h5|.laz", recursive = T)

    plots <- dplyr::select(plots, plotID, siteID, utmZone, easting, northing)
    plots <- unique(plots)
    plots <- plots[complete.cases(plots),]
    plots$plt_e <- as.integer(plots$easting / 1000) * 1000
    plots$plt_n <- as.integer(plots$northing / 1000) * 1000

    #remove tiles from sites not included in the dataset
    list_data <- stringr::str_detect(list_data,
                      paste(unique(plots["siteID"]), collapse = '|'))

    #apply the clipping function to each plot in the dataset
    print(paste("extracting plots information for data in:", dataproduct))
    pbapply(plots, 1, clip_plot, list_data, bff=bff)
  }
}

