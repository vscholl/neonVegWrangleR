#' create a shapefile out of vegetation structure data with lat/lon coordinates
#'
#'
#' @inheritParams str_detect
#' @return A list of dataframe
#' @export
#' @examples apply(plots, 1, clip_plot, list_data, bff=bff)
#' @importFrom magrittr "%>%"
#'
#'
retrieve_cfc_data <- function(site = "all", start = NA, enddate = NA,
                              unwanted = c("validation","variables", "cfc_elements")){
  # download foliar traits data
  fol_fc <- neonUtilities::loadByProduct("DP1.10026.001", check.size=F,
                                         site=site,  start = start, enddate = enddate)

  # for joining only the data we are interested in and cleaning the putput, hardcoding the columns we consider of interest
  col_to_keep <- c("domainID", "siteID", "plotID", "sampleID",
                   "collectDate", "nitrogenPercent", "carbonPercent",
                   "CNratio", "freshMass", "extractChlAConc",
                   "extractChlBConc", "extractCarotConc", "dryMass",
                   "foliarPhosphorusConc", "foliarPotassiumConc",  "foliarCalciumConc",
                   "foliarMagnesiumConc",  "foliarSulfurConc","foliarManganeseConc",
                   "foliarIronConc","foliarCopperConc","foliarBoronConc",  "taxonID",
                   "foliarZincConc", "nlcdClass", "individualID", "plantStatus","ligninPercent",
                   "cellulosePercent", "leafArea", "percentGreen","leafMassPerArea","dryMassFraction")

  #get columns of interest
  df_combined <- list()
  for(dat in names(fol_fc)){
    if(!(dat %in% unwanted)){
      dt <- fol_fc[[dat]] %>%
        dplyr::select(one_of(col_to_keep))
      if(dat == "cfc_chlorophyll"){
        dt <- dplyr::rename(dt, pigments_fresh_weight = freshMass)
      }else if(dat == "cfc_lignin"){
        dt <- dt %>% dplyr::rename(cel_lign_dry_mass = dryMass)
      }else if(dat == "cfc_LMA"){
        dt <- dt %>% dplyr::rename(lma_dry_mass = dryMass)
        dt <- dt %>% dplyr::rename(lma_fresh_mass = freshMass)
      }
      df_combined[[dat]] <- dt
    }
  }
  #combine them all in a single dataframe
  df_combined <- Reduce(dplyr::left_join,
                        list(df_combined[["cfc_fieldData"]], df_combined[["cfc_carbonNitrogen"]],
                             df_combined[["cfc_chlorophyll"]],df_combined[["cfc_lignin"]],
                             df_combined[["cfc_LMA"]])) #, df_combined[["cfc_elements"]]))
  return(df_combined)
}
