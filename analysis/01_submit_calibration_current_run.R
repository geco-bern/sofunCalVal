# submit calibration for multiple folds. All calibration settings and data split
# is specified in fuctions/scripts:
# - src/submit_byfold.sh: calls analysis/rscript_calibrate_byfold.R to start the calibration for an individual fold
# - analysis/rscript_calibrate_byfold.R: reads and split the data and calls the function calibrate_rsofun()
# - calibrate_rsofun(): has all the calibration specification hard-coded and calls the rsofun functions, including calib_sofun()

# install the current version
devtools::install_github(
  "geco-bern/rsofun",
  ref = "HEAD",
  upgrade = "never",
  force = TRUE
)

# submit calibration by fold
system(paste(here::here("src/submit_allfolds.sh"), here::here()))
