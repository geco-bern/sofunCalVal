#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)   # to receive arguments from the shell: number of fold (1-5)
fold <-  as.integer(args[1])
message(paste("Fold ", fold))

source(here::here("R/create_data_split.R"))
source(here::here("R/calibrate_rsofun.R"))

# get pre-determined splits for five folds
splits <- create_data_split()

# load driver data for all sites
drivers <- readRDS(here::here("data/drivers.rds"))

# determine training data subset for this fold
drivers_train <- drivers |>
  dplyr::filter(sitename %in% splits[[fold]]$train_sitename)

# calibrate with this fold's training data
par_calib <- calibrate_rsofun(drivers_train)

# save parameters and model object to file
filnam <- here::here(paste0("data/par_calib_fold_", fold, ".rds"))
message(paste0("Writing parameters to file ", filnam, "..."))
saveRDS(par_calib, file = filnam)
