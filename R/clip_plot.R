#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples from_inventory_to_shp()
#' @importFrom magrittr "%>%"
#' @import sf, reticulate, stringr
clip_plot <- function(plt, list_data, which_python = "pyenv", bff=22){
  # get tile for the plot
  plt <- data.frame(t(plt), stringsAsFactors=F)
  #convert plots coordinates from character to numeric
  plt[["easting"]]<- as.numeric(plt[["easting"]])
  plt[["northing"]]<- as.numeric(plt[["northing"]])

  tile <- paste(plt[["plt_e"]], plt[["plt_n"]], sep="_")
  tile <- grep(tile, list_data, value = TRUE)
  missed_plots <- list()
  #in case of multiple data products per neon product
  for(f in tile){
    #load raster or las file
    if(substr(f, nchar(f)-4+1, nchar(f))==".tif"){
      prd = substr(f, nchar(f)-8+1, nchar(f)-4)
      f<-raster::brick(f)
      #get object with  extent of plot center + buffer
      e <- raster::extent(plt[["easting"]] - bff,
                          plt[["easting"]] + bff,
                          plt[["northing"]] - bff,
                          plt[["northing"]] + bff)
      #crop
      tif <- raster::crop(f, e)
      #and save
      raster::writeRaster(tif,  paste("./outdir/plots/", prd, "/",
                                      plt[1,1], ".tif", sep=""), overwrite=TRUE)
    }else if(substr(f, nchar(f)-4+1, nchar(f))==".laz"){
      #skip files in metadata
      if(!str_detect(f, "Metadata")){
        #read pointcloud
        f <- lidR::readLAS(f)
        #clip by extent
        las <- lasclipRectangle(f, xleft = plt[["easting"]] - bff,
                                ybottom=plt[["northing"]] - bff,
                                xright=plt[["easting"]] + bff,
                                ytop=plt[["northing"]] + bff)
        #and save
        writeLAS(las, paste("./outdir/plots/las/",
                            plt[1,1], ".las", sep=""))
      }
    }else if(substr(f, nchar(f)-3+1, nchar(f))==".h5"){
      #get epsg from h5
      epsg <- get_epsg_from_utm(plt[["utmZone"]])
      #convert h5 into a tif for the extent of the plot using python
      use_condaenv(which_python, required = T)
      #check if the libraries required are installed in the virtual environment
      h5py <- import("h5py")
      source_python("./R/extract_raster_from_h5.py")
      tryCatch({
        h5_to_tif <- extract_hsi(f,
                                 plt[1,1],
                                 plt[["easting"]] - bff,
                                 plt[["easting"]] + bff,
                                 plt[["northing"]] - bff,
                                 plt[["northing"]] + bff,
                                 epsg,
                                 ras_dir = './outdir/plots/hsi/')
      }, error = function(e) {
        missed_plots[[plt[1,1]]] <- c(f, plt[1,1], plt[["easting"]], plt[["northing"]])
      })
    }
  }
  return(missed_plots)
}
