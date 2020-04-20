#pipeline 

#downlload vegetation structure data and retrieve ITC coords
vst_data = retrieve_VST_data(method="shp")

#append ecological information to position
vst_loc = vst$vst_mappping_latlon %>% select(-one_of("uid", "namedLocation", "date"
                                                     , "eventID", "domainID", "plotID", "subplotID"
                                                     ,"remarks", "measuredBy", "recordedBy", "dataQF", "siteID"))
vst_df = inner_join(vst_loc, vst$vst_apparentindividual)

#latest entries
vst_df = vst_df %>% group_by(individualID) %>% top_n(1,wt=eventID)
colnames(vst_df)[34:35] = c("longitude", "latitude")
write_csv(vst_df, "./outdir/field_data/vst_latest.csv")

#get only plants that can eventually be used as trees (taller than 2m and eventually detectable from sky)
vst_df = vst_df %>% filter(height > 4)
write_csv(vst_df, "./outdir/field_data/vst_latest_2m.csv")

vst_df_sun = vst_df %>% filter(!canopyPosition %in% c("Full shade", "Mostly shaded"))
write_csv(vst_df, "./outdir/field_data/vst_latest_sun.csv")

# get AOP data


# append data


