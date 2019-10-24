#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples from_inventory_to_shp()
#' @importFrom magrittr "%>%"
#' @import sf
crop_data_to_plot <- function(dataproduct= "DP3.30025.001", plots){
  #get path of files in
  list_data <- list.files(path = paste("./outdir/", dataproduct, sep=""), full.names = T,
                          pattern = ".tif|.h5|.laz", recursive = T)

  plots$easting <- as.integer(plots$easting / 1000) * 1000
  plots$northing <- as.integer(plots$northing / 1000) * 1000
  lapply(clip_plot, 1:nrow(plots))
}

clip_plot <- function(plt, list_data, bff=22){
  # get tile for the plot
  tile <- paste(plt["easting"], plt["northing"], sep="_")
  tile <- dplyr::filter(data.frame(list_data), grepl(tile, list_data))
  tile <- unlist(tile) %>% as.character
  #in case of multiple data products per neon product
  for(f in tile){
    #load raster or las file
    if(substr(f, nchar(f)-4+1, nchar(f))==".tif"){
      prd = substr(f, nchar(f)-8+1, nchar(f)-4)
      f<-raster::brick(f)
      #get object with  extent of plot center + buffer
      e <- raster::extent(plt$easting - bff,
                  plt$northing - bff,
                  plt$easting + bff,
                  plt$northing + bff)
      #crop
      tif <- raster::crop(f, e)
      #and save
      raster::writeRaster(tif,  paste("./outdir/plots/", prd, "/",
                                      plt[1,1], ".tif", sep=""))
    }else if(substr(f, nchar(f)-4+1, nchar(f))==".laz"){
      #read pointcloud
      f <- lidR::readLAS(f)
      #clip by extent
      las <- lasclipRectangle(f, xleft = plt$easting - bff,
                              plt$northing - bff,
                              plt$easting + bff,
                              plt$northing + bff)
      #and save
      writeLAS(las, paste("./outdir/plots/las/",
                          plt[1,1], ".las", sep=""))
    }else if(substr(f, nchar(f)-4+1, nchar(f))==".h5"){
      #convert h5 into a tif for the extent of the plot using python



    }
  }
}
