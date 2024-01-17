ml_localize_dets_error_fn <- function(tag_f,
                                      output_folder,
                                      node_folder,
                                      grid_points_folder,
                                      log_dist_RSSI_mdl,
                                      tz = tz,
                                      crs = crs,
                                      reps = reps){
  
  cat("############ \n",
      "Starting localizing tag: ", tag_f, "\n",
      "############ \n", sep = "")
  
  ## Read in model and convert to table
  mdl_tab <- readRDS(log_dist_RSSI_mdl) %>%
    broom::augment(newdata = data.frame(rssi = seq(-25, -115, by = -1)),
                   se_fit = TRUE) %>%
    dplyr::select(mean_rssi = rssi,
                  mean = .fitted,
                  sd = .se.fit) %>%
    dplyr::distinct(mean_rssi,.keep_all = T)
  
  ## Convert grid points to lat/long  ########
  grid_points_ll <- suppressWarnings(grid_points %>%
                                       sf::st_as_sf(coords = c("gp_x","gp_y"),
                                                    crs = crs) %>%
                                       sf::st_transform(4326) %>%
                                       dplyr::transmute(grid_point,
                                                        gp_lon = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                        gp_lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
                                       sf::st_drop_geometry())
  
  ## Create directory if needed
  if(!dir.exists(paste0(output_folder,"ml_localized/w_error/15s/", tag_f))){
    
    suppressWarnings(dir.create(paste0(output_folder,"ml_localized/w_error/15s/")))
    dir.create(paste0(output_folder,"ml_localized/w_error/15s/", tag_f))  
  }
  
  ## Prepared files
  files_prep <- list.files(path = paste0(output_folder,
                                         "ml_prepared/w_error/15s/",
                                         tag_f,
                                         "/"),
                           ".csv.gz")
  
  ## Get files that have been localized
  files_localized <- list.files(path = paste0(output_folder,
                                              "ml_localized/w_error/15s/",
                                              tag_f,
                                              "/"),
                                ".csv.gz")
  
  ## Files to localize
  files_2_localize <- paste0(output_folder,
                             "ml_prepared/w_error/15s/",
                             tag_f,
                             "/",
                             files_prep[!(files_prep %in% files_localized)])
  
  
  ## If any files
  if(length(files_2_localize) > 0){
    
    
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
        tidyr::pivot_longer(cols = matches("^gp|^Pp"),
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
        
        ## For each interval
        for(int in unique(dets_p$t_ind)){
          
          # int = unique(dets_p$t_ind)[1]
          
          ## Progress bar
          Sys.sleep(0.1)
          setTxtProgressBar(pb_ints, which(unique(dets_p$t_ind) == int))
          
          tryCatch(
            expr = {
              
              ## Subset detections
              dets_p_int = dets_p[dets_p$t_ind == int,]
              
              ## Ellipse info
              ellipse_center_est <- list()
              ellipse_cov_est <- list()
              
              ## Repeat X times
              for(i in 1:reps){
                
                ## Sample within interval - SD set to 0
                dets_p_int_sample <- dets_p_int %>%
                  dplyr::rowwise() %>%
                  dplyr::mutate(dist_est_samp = round(10^(sample(rnorm(n = 100, mean = mean,sd = sd),size = 1))),
                                
                                ## Cutoff estimates at 150 m
                                dist_est_samp = ifelse(dist_est_samp > 150, 150, dist_est_samp))
                
                # Determine the node with the strongest avg.RSSI value to be used as starting values
                max_RSSI <- dets_p_int[which.max(dets_p_int$mean_rssi),]
                
                # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on RSSI values (distance) and the pairwise distance between all nodes
                distHaversine <- geosphere::distHaversine
                nls_mod <- suppressWarnings(nls(dist_est_samp ~ geosphere::distm(data.frame(gp_lon, gp_lat),
                                                                                 c(lng_solution, lat_solution),
                                                                                 fun = distHaversine), # distm - matrix of pairwise distances between lat/longs
                                                data = dets_p_int_sample,
                                                start = list(lng_solution = max_RSSI$gp_lon,
                                                             lat_solution = max_RSSI$gp_lat),
                                                control = nls.control(warnOnly = T,
                                                                      minFactor=1/30000)))
                
                ## Determine error around the point location estimate
                ellipse <- car::confidenceEllipse(nls_mod,
                                                  levels = 1-exp(-1),
                                                  segments = 1000,
                                                  draw = F)
                
                ## Project
                ellipse_coords_proj <- ellipse %>%
                  data.frame() %>%
                  sf::st_as_sf(coords = c("x", "y")) %>%
                  sf::st_set_crs(4326) %>%
                  sf::st_transform(crs) %>%
                  dplyr::transmute(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                   y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
                  sf::st_drop_geometry()
                
                ## Ellipse error features
                ell.info <- cov.wt(ellipse_coords_proj)
                
                ## Add ellipse info to list
                ellipse_center_est[[i]] <- ell.info$center
                ellipse_cov_est[[i]] <- ell.info$cov
                
              }
              
              
              ## Get average point
              pts <- do.call(rbind, ellipse_center_est) %>%
                as.data.frame()
              mean_pt_est <- data.frame(x = mean(pts$x),
                                        y = mean(pts$y))
              
              
              ## Get mean covariance matrix
              mean_cov <- apply(simplify2array(ellipse_cov_est), c(1,2), mean)
              eigen.info <- eigen(mean_cov)
              
              ## Axes
              e <- sqrt(eigen.info$values)
              a <- sqrt(e[1]/2)  # semi-major axis
              b <- sqrt(e[2]/2)  # semi-minor axis
              # error_ellipse_area = round(a*b*pi, 0) # area of error ellipse
              
              ## Orientation
              u <- eigen.info$vectors[,1] # major axis eigenvector
              theta <- atan2(u[2],u[1]) # angle from x-axis in radians
              theta <- theta *360/(2*pi) # angle from x-axis in degrees
              theta <- theta - 90 # angle from y-axis in degrees
              
              ## Combine estimated locations with summary information
              tag_int_loc_est <- data.frame(tag = tag_f,
                                            dt_r = dets_p_int$dt_r[1],
                                            n_gp = dets_p_int$n_gp[1],
                                            mean_RSSI = mean(dets_p_int$mean_rssi),
                                            max_RSSI = max(dets_p_int$mean_rssi),
                                            x_est = mean_pt_est$x,
                                            y_est = mean_pt_est$y,
                                            semi_major = a,
                                            semi_minor = b,
                                            orientation = theta)
              
              ## Combine
              tag_loc_est <- rbind(tag_loc_est, tag_int_loc_est)
            },
            error = function(e){
              "Too few detections to trilaterate location"
            }
            
          )
          
          close(pb_ints)
          
        }
        cat("\n Save as seperate file")
        
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
    
    cat("############ \n",
        "Finished localizing tag: ", tag_f, "\n",
        "############ \n", sep = "")
    
  }
  
  ## End progress bar for files
  close(pb2)
  
  cat("############ \n",
      "Finished localizing tag: ", tag_f, "\n",
      "############ \n", sep = "")
  
  
}
