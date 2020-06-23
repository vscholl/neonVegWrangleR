#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples apply(plots, 1, clip_plot, list_data, bff=bff)
#' @importFrom magrittr "%>%"
#' @import sf, reticulate
crop_data_to_plot <- function(plt, dtprd= c("DP3.30006.001","DP3.30010.001", "DP1.30003.001"
                                      # ,"DP3.30024.001",  "DP3.30025.001",  "DP3.30015.001", "DP1.30003.001", 
                                      ),path = "//orange/ewhite/s.marconi/brdf_traits/corrHSI/", #"//orange/ewhite/s.marconi/AOP_Chapter3/", #path = "//orange/idtrees-collab/hsi_brdf_corrected/corrHSI/", #path = "./outdir/tiles/",
                              which_python = "/home/s.marconi/.conda/envs/quetzal3/bin/python", target_year = 2018,
                              bff=20){
  #get path of files in
  library(tidyverse)
  library(parallel)
  library(reticulate)
  library(neonUtilities)
  library(sf)
  full_list <- list.files(path = path, full.names = T, #paste(path, dataproduct, sep="")
                          pattern = ".tif|.h5|.laz", recursive = T)
  for(dataproduct in dtprd){
    # list_data <- list.files(path = path, full.names = T, #paste(path, dataproduct, sep="")
    #                         pattern = ".tif|.h5|.laz", recursive = T)

    # paths_to_loop <- stringr::str_detect(full_list, dataproduct)
    # list_data <- full_list[paths_to_loop]
    # paths_to_loop <- stringr::str_detect(list_data, as.character(target_year))
    # list_data <- list_data[paths_to_loop]
    list_data =   full_list 
    
    plots <- dplyr::select(plt, plotID, siteID, utmZone, easting, northing)
    plots <- plots %>% group_by(plotID, siteID, utmZone) %>% summarize_if(is.numeric, mean)
    colnames(plots)[4:5] <- c("easting", "northing")
    plots <- plots[complete.cases(plots),]
    plots$plt_e <- as.integer(plots$easting / 1000) * 1000
    plots$plt_n <- as.integer(plots$northing / 1000) * 1000

    # create subplots for those at the intersection among tiles
    plots$check_e = floor(plots$easting- plots$plt_e)
    plots$check_n = floor(plots$northing - plots$plt_n)
    
    tile_ep = plots %>% filter(check_e < bff) %>%
      ungroup() %>%
      mutate(plt_e = plt_e -1000, 
             plotID = paste(plotID, "pl", sep=""),
             easting = easting - bff- check_e)
    tile_em = plots %>% filter(check_e >1000-bff) %>%
      ungroup() %>%
      mutate(plt_e = plt_e +1000, plotID = paste(plotID, "mn", sep=""),
             easting = easting + bff-check_e+1000)

    tile_np = plots %>% filter(check_n < bff) %>%
      ungroup() %>%
      mutate(plt_e = plt_e -1000, plotID = paste(plotID, "pl", sep=""),
             northing = northing - bff- check_n)
    tile_nm = plots %>% filter(check_n >1000-bff) %>%
      ungroup() %>%
      mutate(plt_n = plt_n +1000, plotID = paste(plotID, "mn", sep=""),
             northing = northing - bff- check_n)
    plots$easting = plots$plt_e + pmax(bff, plots$check_e)
    plots$northing = plots$plt_n + pmax(bff, plots$check_n)
    
    #get borders
    border_e = floor(plots[plots$check_e>1000-bff,"easting"]/1000)*1000
    plots[plots$check_e>1000-bff,"easting"] = border_e+1000-bff
    border_n = floor(plots[plots$check_n>1000-bff,"northing"]/1000)*1000
    plots[plots$check_n>1000-bff,"northing"]= border_n+1000-bff
    
    plots = rbind.data.frame(plots, tile_ep, tile_em, tile_np, tile_nm)
    
    #remove tiles from sites not included in the dataset
    paths_to_loop <- stringr::str_detect(list_data,
                      paste(unique(plots[["siteID"]]), collapse = '|'))
    list_data <- list_data[paths_to_loop]
    #apply the clipping function to each plot in the dataset
    print(paste("extracting plots information for data in:", dataproduct))
    #pbapply(plots, 1, clip_plot, list_data, which_python = which_python, bff=bff)
    cl= makeCluster(12)
    parApply(cl = cl, plots, 1, clip_plot, list_data, which_python = which_python, bff=bff)
    stopCluster(cl)
  }
}

