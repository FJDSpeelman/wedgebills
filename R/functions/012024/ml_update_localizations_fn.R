## Update localizations using multilateration
ml_update_localizations_fn <- function(tag_f = as.character(),
                                       db_name = as.character(),
                                       db_password = as.character(),
                                       tag_folder = as.character(),
                                       node_folder = as.character(),
                                       grid_points_folder = as.character(),
                                       output_folder = as.character(),
                                       log_dist_RSSI_mdl = as.character(),
                                       tz = "UTC",
                                       crs = as.numeric(),
                                       reps = as.numeric()){


  ## Prepare each tag
  dets_t <- collect_raw_data_fn(db_name = db_name,
                      db_password = db_password,
                      tag_folder = tag_folder,
                      node_folder = node_folder,
                      grid_points_folder = grid_points_folder,
                      output_folder = output_folder,
                      tag_f = tag_f,
                      tz = tz)


  ## If new data to prepare:
  if(nrow(dets_t) > 0){
    
    ## Prepare tag
    ml_prepare_dets_error_fn(tag_f = tag_f,
                             dets_t = dets_t,
                             grid_points = grid_points,
                             output_folder = output_folder,
                             tz = tz)
  
  
  ## Then localize
  ml_localize_dets_error_fn(tag_f = tag_f,
                            output_folder = output_folder,
                            log_dist_RSSI_mdl = log_dist_RSSI_mdl,
                            tz = tz,
                            crs = crs,
                            rep = reps)
  } else{
    
    cat("############ \n",
        "No new data to prepare for tag: ", tag_f, "\n",
        "############ \n", sep = "")
    
  }
  
}
