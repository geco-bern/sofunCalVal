#!/usr/bin/env Rscript

# This script runs the calibration for one single "fold." It reads the full rsofun
# driver data, subsets it for the training set of this fold, calls the
# calibration function, and writes its output to file.

# install current verision of rsofun
if ("rsofun" %in% installed.packages()) remove.packages("rsofun")
devtools::install_github(
  "geco-bern/rsofun",
  ref = "HEAD",
  upgrade = "never",
  force = TRUE
)

library(tidyverse)
library(rsofun)

source(here::here("R/calibrate_rsofun.R"))

# load driver data for all sites
drivers <- readRDS(here::here("data/drivers.rds"))

# calibrate with this fold's training data
par_calib <- calibrate_rsofun(drivers)

# save parameters and model object to file
filnam <- here::here(paste0("data/par_calib_current.rds"))
message(paste0("Writing parameters plus trace to file ", filnam, "..."))
saveRDS(par_calib, file = filnam)


# light version: only parameters, not trace
filnam <- here::here(paste0("data/par_calib_light_current.rds"))
message(paste0("Writing parameters only to file ", filnam, "..."))
saveRDS(par_calib$par, file = filnam)
