## Update localizations using multilateration
ml_update_localizations_fn <- function(tag_f = as.character(),
                                       db_name = as.character(),
                                       db_user = as.character(),
                                       db_password = as.character(),
                                       project = as.character(),
                                       tag_folder = as.character(),
                                       node_folder = as.character(),
                                       grid_points_folder = as.character(), 
                                       output_folder = as.character(),
                                       log_dist_RSSI_mdl = as.character(),
                                       tz = "UTC",
                                       crs = as.numeric()){
  
  cat("\n Starting to collect raw data for tag:", tag_f, "\n")
  
  ## Connect to data base back end
  conn <- DBI::dbConnect(RPostgres::Postgres(),
                         dbname = db_name,
                         password = db_password)
  cat("\n Connected to database \n")
  
  ## Get most recent date file (if it exists)
  mrdf <- rev(list.files(paste0(output_folder,"ml_prepared/w_error/15s/",tag_f,""),full.names = TRUE, pattern = ".csv.gz"))[1] #select recent file, as compessed csv
  cat("\n Localized recent data file \n")
  
  ## Get date time to filter by
  if(!is.na(mrdf)){
    mrd <- suppressWarnings(readr::read_csv(mrdf,show_col_types = FALSE) %>%
                              dplyr::pull(dt_r) %>%
                              max())
    mrd <- lubridate::with_tz(mrd, tz = tz)
  } else{
    mrd <- as.Date("2023-09-01")
  }
  cat("\n Found filter date:", as.character(mrd), "\n")
  
  ## Read in tag log and reformat
  tag_log_mr <- sort(list.files(paste0(tag_folder),
                                full.names = TRUE,
                                pattern = "tag_log"),
                     decreasing = TRUE)[1]
  
  cat("\n Read tag log \n")
  
  ## Read in detections from database
  dets_f <- dplyr::tbl(conn, from = "raw") %>%
    dplyr::filter(tag_id %in% local(tag_f)) %>%
    #dplyr::filter(time > local(tag_start_dt)) %>% #not needed for first import
    #dplyr::filter(time > local(mrd)) %>% #not needed for first import
    dplyr::collect() %>%
    dplyr::transmute(tag = tag_id,
                     date_time = lubridate::with_tz(time, tz = tz),
                     node = toupper(node_id),
                     rssi = tag_rssi) %>%
    dplyr::distinct(tag,
                    node,
                    date_time,
                    .keep_all = T)
  cat("\n Read in db detections \n")
  
  ## Read in node codes
  node_codes <- readxl::read_xlsx(paste0(node_folder,"node_codes_20230906.xlsx")) %>%
    dplyr::mutate(node_number = as.character(node_number))
  
  ## Read in node log and reformat
  node_log_mr <- sort(list.files(paste0(node_folder),
                                 full.names = TRUE,
                                 pattern = "node_deployment_log"),
                      decreasing = TRUE)[1]
  
  node_log <- suppressWarnings(readxl::read_excel(path = node_log_mr) %>%
                                 dplyr::mutate(deployment_time = lubridate::parse_date_time(paste(start_date, start_time), "dmy HM", tz = tz),
                                               removal_time = lubridate::parse_date_time(paste(end_date, end_time), "dmy HM", tz = tz)) %>%
                                 ## Join node node
                                 dplyr::left_join(node_codes,
                                                  by  = "node_number") %>%
                                 
                                 dplyr::select(node,
                                               grid_point,
                                               date_time = deployment_time,
                                               removal_time))
  ## Convert to data.table
  nodes <- data.table::data.table(node_log, key = c("node", "date_time"))
  dets_f <- data.table::data.table(dets_f, key = c("node", "date_time"))
  
  ## Rolling join node log to node records
  dets_f <- nodes[dets_f, roll = Inf]
  
  ## Remove locations without a grid point and where date time is after removal time
  dets_f1 <- dets_f %>%
    dplyr::filter(!is.na(grid_point),
                  (date_time < removal_time | is.na(removal_time))) %>%
    
    ## Remove impossible RSSI values
    dplyr::filter(rssi < 0) %>%
    
    dplyr::arrange(tag,
                   date_time) %>%
    dplyr::select(grid_point,
                  tag,
                  date_time,
                  rssi) %>%
    data.frame()
  
  ## Add day column
  dets_t <- dets_f1 %>%
    dplyr::mutate(date = lubridate::floor_date(date_time, unit = "day"),
                  grid_point = paste0("gp_", grid_point))  %>%
    dplyr::filter(!grepl("d",grid_point))
  
  ## Get grid point coordinates
  grid_points <- suppressWarnings(sf::read_sf(paste0(grid_points_folder, "grid_points.kml")) %>%
                                    sf::st_transform(3308) %>%
                                    dplyr::transmute(grid_point = gsub("Gp ", "gp_", Name),
                                                     x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                     y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
                                    sf::st_drop_geometry())
  
  
  cat("############ \n",
      "Finished collecting raw data for tag
      ############ \n", sep = "")
  
  ## Prepare each tag
  ml_prepare_dets_error_fn(tag_f = tag_f,
                           dets_t = dets_t,
                           grid_points = grid_points,
                           output_folder = output_folder,
                           tz = tz)
  
  cat("############ \n",
      "Finished preparing tag: ", tag_f, "\n",
      "############ \n", sep = "")
  
  ## Then localize
  ml_localizing_fn(tag_f = tag_f,
                   output_folder = output_folder,
                   grid_points = grid_points,
                   log_dist_RSSI_mdl = log_dist_RSSI_mdl,
                   tz = tz,
                   crs = crs)
  
  cat("############ \n",
      "Finished localizing tag: ", tag_f, "\n",
      "############ \n", sep = "")
  
}
