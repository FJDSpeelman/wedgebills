## Update localizaitons using multilateration method
library(foreach)
## Source functions
source("./R/functions/ml_update_localizations_fn.R")
source("./R/functions/ml_prepare_dets_error_fn.R")
source("./R/functions/ml_localize_dets_error_fn.R")

cl <- parallel::makeForkCluster(8, outfile = "")
doParallel::registerDoParallel(cl)


tags <- list.files("G:/My Drive/PhD/Wedgebills/CTT_data_processing/CTT_data/CTT_data_process/R/data/Wedgebill_tags/")[2] #find my tags from excel
tag_f <- readxl::read_excel(paste0("./R/data/Wedgebill_tags/",tags))$Tag #vector of tag IDs in latest file


foreach(tag_f=tag_f[1],.packages=c("tidyverse","lubridate","readr","geosphere"),
        .verbose = TRUE) %dopar%
  { ml_update_localizations_fn(db_name = "radiotracking",
                               db_user = "postgres",
                               db_password = "postgres",
                               project = "zebby_tracking",
                               tag_f = tag_f,
                               node_folder = "./R/data/nodes/",
                               tag_folder = "./R/data/Wedgebill_tags/",
                               output_folder = "./data/processed_detections/ml/",
                               grid_points_folder = "./R/data/grid_points/",
                               log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
                               tz = "Australia/Broken_Hill") }

foreach(tag_f=tag_f[1],.packages=c("tidyverse","lubridate","readr","geosphere"), 
        .verbose = TRUE) %dopar% 
  { ml_localizing_fn(tag_f = tag_f,
                     output_folder = "./data/processed_detections/ml/",
                     node_folder = "./R/data/nodes/",
                     grid_points_folder = "./R/data/grid_points/",
                     log_dist_RSSI_mdl = "./R/data/RSSI_log_dist_model_zebby.RDS",
                     tz = "Australia/Broken_Hill")
  }
