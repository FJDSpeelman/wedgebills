ml_localizing_fn <- function(tag_f,
                             output_folder,
                             node_folder,
                             grid_points_folder,
                             log_dist_RSSI_mdl,
                             tz = tz,
                             crs = crs){
  
  log_dist_RSSI_mdl <- readRDS(log_dist_RSSI_mdl)
  
  ## Read in model
  deg.f <- log_dist_RSSI_mdl$df.residual
  
  print("read model")
  
  ## Convert to table
  mdl_tab <- log_dist_RSSI_mdl %>% 
    broom::augment(newdata = data.frame(rssi = seq(-25, -115, by = -1)),
                   se_fit = TRUE,
                   interval = "confidence") %>% 
    dplyr::transmute( mean_rssi = rssi,
                      mean = .fitted,
                      sd = sqrt(.se.fit * deg.f)) %>% 
    dplyr::distinct(mean_rssi,.keep_all = T)
  
  print("converted to table")
  
  # Get grid point coordinates
  grid_points <- suppressWarnings(sf::read_sf(paste0(grid_points_folder, "grid_points.kml")) %>%
                                    sf::st_transform(3308) %>%
                                    dplyr::transmute(grid_point = gsub("Gp ", "gp_", Name),
                                                     x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                     y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
                                    sf::st_drop_geometry())

  print("got gp coordinates")
  
  ## Convert grid points to lat/long  ########
  grid_points_ll <- suppressWarnings(grid_points %>%
                                       sf::st_as_sf(coords = c("x","y"),
                                                    crs = 3308) %>% 
                                       sf::st_transform(4326) %>% 
                                       dplyr::transmute(grid_point,
                                                        gp_lon = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                        gp_lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
                                       sf::st_drop_geometry())
  
  ## Create directory if needed
  if(!dir.exists(paste0(output_folder,"ml_localized/w_error/15s/", tag_f))){
    
    dir.create(paste0(output_folder,"ml_localized/w_error/15s/", tag_f))  
  }
  
  ## Prepared files
  files_prep <- list.files(path = paste0(output_folder,
                                         "/ml_prepared/w_error/15s/",
                                         tag_f,
                                         "/"),
                           ".csv.gz")
  
  ## Get files that have been localized
  files_localized <- list.files(path = paste0(output_folder,
                                              "/ml_localized/w_error/15s/",
                                              tag_f,
                                              "/"),
                                ".csv.gz")
  ## If any files
  if(length(files_prep[!(files_prep %in% files_localized)]) > 0){
    
    ## Files to localize
    files_2_localize <- paste0(output_folder,
                               "/ml_prepared/w_error/15s/",
                               tag_f,
                               "/",
                               files_prep[!(files_prep %in% files_localized)])
    
    
    
    cat("\n Starting tag:", tag_f, " - ", length(files_2_localize), "days to localize", "\n")
    
    ## Set progress bar
    pb2 <- txtProgressBar(min = 0, max = length(files_2_localize), style = 3)
    
    ## Process each file for each tag separately ######
    for (file in files_2_localize){
      
      # file <- files_2_localize[1]
      
      ## Get current date
      tag_f_date <- stringi::stri_sub(gsub(".csv.gz","",file),-10)
      
      ## Progress bar
      Sys.sleep(0.1)
      setTxtProgressBar(pb2, which(files_2_localize == file))
      
      ## Get detections
      dets <- readr::read_csv(file,
                              show_col_types = FALSE,
                              guess_max = 10000)
      
      
      ## Process for nls 
      dets_p <- dets %>% 
        
        dplyr::select(-dets) %>% 
        
        dplyr::group_by(dt_r) %>% 
        tidyr::pivot_longer(cols = contains("gp_"),
                            names_to = "grid_point",
                            values_to = "mean_rssi") %>% 
        na.omit() %>% 
        
        ## Remove any periods with fewer than 3 gps
        dplyr::filter(n_gp >= 3) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(t_ind = cumsum(!duplicated(dt_r))) %>% 
        
        ## Join node pts
        dplyr::left_join(grid_points_ll,
                         by = "grid_point") %>% 
        ## Join model interval
        dplyr::left_join(mdl_tab,
                         by = "mean_rssi")
      
      ## If any:
      if(nrow(dets_p) > 0){
        
        cat("\n Starting tag:", tag_f, "- date:", tag_f_date, "- intervals to localize:", length(unique(dets_p$t_ind)), "\n")
        
        ## Starting time
        start_time <- Sys.time()
        
        ## Empty df
        tag_loc_est <- data.frame()
        
        ## Set progress bar
        pb_ints <- txtProgressBar(min = 0, max = 100, style = 3)
        # 
        ## For each interval
        for(int in unique(dets_p$t_ind)){
          
          # int = unique(dets_p$t_ind)[10]
          
          ## Progress bar
          Sys.sleep(0.1)
          setTxtProgressBar(pb_ints, which(unique(dets_p$t_ind) == int))
          # 
          ## Progress bar
          Sys.sleep(0.1)
          setTxtProgressBar(pb, which(unique(dets_p$t_ind) == int))
          
          tryCatch(
            expr = {
              
              ## Subset detections
              dets_p_int = dets_p[dets_p$t_ind == int,]
      
              ## Repeat x times
              for(i in 1:500){
                
                ## Sample within interval
                dets_p_int_sample <- dets_p_int %>%
                  dplyr::rowwise() %>%
                  dplyr::mutate(dist_est_sample = round(10^(sample(rnorm(n = 100, mean = mean,sd = sd),size = 1))),
                                
                                ## Cutoff estimates at 150 m
                                dist_est_sample = ifelse(dist_est_sample > 150, 150, dist_est_sample))
                
                # Determine the node with the strongest avg.RSSI value to be used as starting values
                max_RSSI <- dets_p_int_sample[which.max(dets_p_int$mean_rssi),]
                
                ## Non-linear test to optimize the location of unknown signal
                nls_mod <- suppressWarnings(nls(dist_est_sample ~ raster::pointDistance(data.frame(gp_x, gp_y),
                                                                                        c(x_solution, y_solution),
                                                                                        lonlat = F, allpairs = T), # distm - matrix of pairwise distances between lat/longs
                                                data = dets_p_int_sample,
                                                start = list(x_solution = max_RSSI$gp_x,
                                                             y_solution = max_RSSI$gp_y),
                                                control = nls.control(warnOnly = T,
                                                                      minFactor=1/30000)))
                
              }
              
              ## Get ellipse features
              ee_features<- suppressWarnings(pts %>%
                                               # mutate(rep = 1) %>%
                                               nest() %>%
                                               mutate(ellipse = lapply(data, function(x) cluster::ellipsoidhull(cbind(x$x,
                                                                                                                      x$y))),
                                                      x_center = unlist(lapply(ellipse, function(x) x$loc[1])),
                                                      y_center = unlist(lapply(ellipse, function(x) x$loc[2])),
                                                      cov = lapply(ellipse, function(x) x$cov),
                                                      vec = lapply(ellipse, function(x) eigen(x$cov)),
                                                      u = lapply(vec, function(x) x$vectors[,1]),
                                                      # d2 = unlist(lapply(ellipse, function(x) x[[3]])),
                                                      e1 = unlist(lapply(ellipse, function(x) sqrt(eigen(x$cov)$values)[1])),
                                                      e2 = unlist(lapply(ellipse, function(x) sqrt(eigen(x$cov)$values)[2])),
                                                      theta = unlist(lapply(u, function(x) (atan2(x[2],x[1])*360/(2*pi)) - 90)),
                                                      a = sqrt(2 * e1),  # semi-major axis
                                                      b = sqrt(2 * e2)) %>%  # semi-minor axis)
                                               select(x_center,y_center,cov,a,b,theta, cov) %>%
                                               mutate(a = ifelse(a == 0, NA, a),
                                                      b = ifelse(b == 0, NA, b)) %>%
                                               na.omit())
              
              ## Combine estimated locations with summary information
              tag_int_loc_est <- data.frame(tag = as.character(dets_p_int$tag[1]),
                                            dt_r = dets_p_int$dt_r[1],
                                            n_gp = dets_p_int$n_gp[1],
                                            mean_RSSI = mean(dets_p_int$mean_rssi),
                                            max_RSSI = max(dets_p_int$mean_rssi),
                                            x_est = ee_features$x_center,
                                            y_est = ee_features$y_center,
                                            cov = paste(as.numeric(unlist(ee_features$cov)), collapse = ","),
                                            a = ee_features$a,
                                            b = ee_features$b,
                                            theta = ee_features$theta)
              
              ## Combine
              tag_loc_est <- rbind(tag_loc_est, tag_int_loc_est)
            },
            error = function(e){ 
              cat(int, ": too few detections to trilaterate location \n")
            }
          )
          
          close(pb_ints)
          
        }
        
        ## Save as separate file
        readr::write_csv(tag_loc_est,
                         paste0(output_folder, 
                                "ml_localized/w_error/15s/",
                                tag_f,
                                "/",
                                tag_f_date,
                                ".csv.gz"))
        
        cat("\n Finished tag:", tag_f, "- date:", tag_f_date, "-", length(unique(dets_p$t_ind)), "intervals localized", 
            "after", round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "minutes \n")
        
        
      } else {
        
        cat("\n No intervals to localize, skipped file")
        
      }
      
    } 
    
  } else{
    
    cat("\n No new files to localize, skipped tag:", tag_f)
    
  }
  
  ## End progress bar for files
  close(pb2)
  
}
