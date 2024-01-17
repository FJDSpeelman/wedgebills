## Update localizaitons using multilateration method
library(foreach)
library(dplyr)

setwd("/Users/friggspeelman/wedgebills") #if needed

## Source functions
source("./R/functions/ml_update_localizations_fn.R")
source("./R/functions/collect_raw_data_fn.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn.R")

## Set up parallelization options
no_cores <- detectCores(logical = T)
no_cores 
cl <- parallel::makeForkCluster(no_cores, outfile = "")
doParallel::registerDoParallel(cl)

## Define the tags to be processed
tags <- readxl::read_xlsx("./R/data/tags/wedgebill_tag_log_20231029.xlsx") %>% #latest wedgebill tag log
  filter(Species == "CW") %>% 
  pull(Tag)

## Grid points 
grid_points_mr <- sort(list.files(paste0(grid_points_folder),
                                  full.names = TRUE,
                                  pattern = "points"),
                       decreasing = TRUE)[1]


## Get grid point coordinates
grid_points <- suppressWarnings(sf::read_sf(grid_points_mr) %>% 
                                  sf::st_transform(crs) %>% 
                                  janitor::clean_names() %>%
                                  dplyr::transmute(grid_point = gsub("Gp ", "gp_", name),
                                                   gp_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                                                   gp_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
                                  sf::st_drop_geometry())

foreach(tag_f=tags,.packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar% #if run on Windows without parallel, use %do%
  { ml_update_localizations_fn(
    
    ## Database credentials
    db_name = "CTT_wedgebills",
    db_password = "postgres",
    
    ## Tag value is defined in foreach function
    tag_f = tag_f,
    
    ## Folder where node logs are saved
    node_folder = "./R/data/nodes/",
    
    ## Folder where tag logs are saved
    tag_folder = "./R/data/tags/",
    
    ## Folder where grid point files are saved
    grid_points_folder = "./R/data/grid_points",
    
    ## Folder where the data should be saved. A new folder will be created for each tag
    output_folder =   "./data/processed_detections/ml/",
    
    ## Location of log-linear model RSSI~distance output
    log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
    
    ## Time zone
    tz = "Australia/Broken_Hill",
    
    ## Projected CRS to use
    crs = 3308,
    
    ## Number of repetitions for resampling localization to estimate error
    reps = 100) }

