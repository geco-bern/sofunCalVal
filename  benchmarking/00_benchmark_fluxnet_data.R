# This code calibrates P-model GPP for X number of
# fluxnet sites, and returns the results for
# benchmarking purposes.

# libraries ----

library(rsofun)
library(dplyr)
library(readr)
library(ingestr)
library(ggplot2)
library(knitr)

# read in benchmarking driver data -----
load("data/xxx.rda")

# set calibration settings ----
settings_calib <- list(
  method              = "gensa",
  targetvars          = c("gpp"),
  timescale           = list( gpp = "d" ),
  maxit               = 5,
  sitenames           = calibsites,
  metric              = "rmse",
  dir_results         = "./",
  name                = "FULL",
  par                 = list( kphio       = list( lower=0.03, upper=0.1, init= 0.05 ),
                              soilm_par_a = list( lower=0.0,  upper=1.0, init=0.0 ),
                              soilm_par_b = list( lower=0.0,  upper=1.5, init=0.6 ) )
 )

# calibrate model parameters ----
set.seed(1982)
settings_calib <- calib_sofun(
  df_drivers = dplyr::filter(df_drivers_fluxnet2015, sitename %in% calibsites),  # use only one site
  ddf_obs = ddf_fluxnet_gpp,
  settings = settings_calib
  )

# print the best parameters ----
print(settings_calib$par_opt)
save(settings_calib, file = "./settings_calib.Rdata")

# update default model parameters ----
params_modl <- list(
	kphio           = 0.05,
	soilm_par_a     = 1.0,
	soilm_par_b     = 0.0,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
	)
params_modl <- update_params(params_modl, settings_calib)

# run the model with the paramters ----
df_output <- runread_pmodel_f(
     df_drivers_fluxnet2015,
     params_modl = params_modl,
     makecheck = TRUE,
     parallel = FALSE
     )

save(df_output, file = "~/data/rsofun_benchmarking/df_output_v3.4.2.Rdata")

evalsites <- df_output %>%
  mutate(ntsteps = purrr::map_dbl(data, ~nrow(.))) %>%
  dplyr::filter(ntsteps > 0) %>%
  pull(sitename)



settings_eval <- list(
  benchmark = list( gpp = c("fluxnet") ),
  sitenames = evalsites,
  agg       = 8  # An integer specifying the number of days used to
            #define the width of bins for daily data aggregated to several days
  )

out_eval <- eval_sofun(
  df_output,
  settings_eval,
  settings_sims,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
  )

out_eval$gpp$fluxnet$plot$gg_modobs_xdaily
out_eval$gpp$fluxnet$plot$gg_modobs_spatial_annual


df_output_allsites <- runread_pmodel_f(
     df_drivers_fluxnet2015,
     params_modl = params_modl,
     makecheck = TRUE,
     parallel = FALSE
     )
