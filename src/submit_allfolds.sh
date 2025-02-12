#!/bin/bash

# This shell script is used to submit all individual jobs for calibration by fold.

nfolds=5
for ((n=1;n<=${nfolds};n++)); do
  # ETH-Euler HPC:
  # echo "Submitting chunk number $n ..."
  # bsub -W 72:00 -u bestocke -J "job_name $n" -R "rusage[mem=10000]" "Rscript vanilla analysis/add_loess_bychunk.R $n $njobs $nlon"

  # Local machine:
  # Calibrating by fold
  $1/analysis/rscript_calibrate_byfold.R $n

done
