# This code calibrates P-model GPP for X number of
# fluxnet sites, and returns the results for
# benchmarking purposes.

# libraries ----

library(tidyverse)
library(rsofun)
library(dplyr)
library(readr)
library(ingestr)
source("R/eval_sofun.R")
source("R/get_stats.R")

# read in benchmarking driver data -----
load("data/df_drivers_fluxnet2015_allsites.rda")
load("data/obs_eval_fluxnet2015.rda")
flue_sites <- readr::read_csv( "data/flue_stocker18nphyt.csv" ) %>%
  dplyr::filter( !is.na(cluster) ) %>%
  distinct(site) %>%
  pull(site)

calibsites <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(!(sitename %in% c("DE-Akm", "IT-Ro1"))) %>%
  dplyr::filter(sitename != "FI-Sod") %>%
  dplyr::filter( c4 %in% c(FALSE, NA) & classid != "CRO" & classid != "WET" ) %>%
  dplyr::filter( sitename %in% flue_sites ) %>%
  pull(sitename)

drivers <- df_drivers_fluxnet2015_allsites %>%
  filter(
    sitename %in% flue_sites
  ) %>%
  filter(
    sitename %in% calibsites
  ) %>%
  rename(
    "site_info" = "siteinfo",
    "params_soil" = "df_soiltexture"
  )

# calibrate model parameters ----
# set.seed(1982)
# settings <- list(
#   method              = "gensa",
#   targets             = c("gpp"),
#   sitenames           = calibsites,
#   metric              = rsofun::cost_rmse_fullstack,
#   control = list(
#     sampler = "DEzs",
#     settings = list(
#       nrChains = 1,
#       burnin = 1,
#       iterations = 4
#     )
#   ),
#   par = list(
#     a = list(lower=0.04, upper=0.09, init=0.05),
#     b = list(lower=0.5, upper=5, init=3.5),
#     c = list(lower=2, upper=5, init=3.5)
#   )
# )
#
# drivers <- p_model_fluxnet_drivers %>%
#   dplyr::filter(sitename %in% calibsites)
#
# estimated_parameters <- rsofun::calib_sofun(
#   drivers = drivers,
#   obs = p_model_fluxnet_calval,
#   settings = settings
#   )

params_modl <- list(
  kphio           = 0.09423773, # 0.08791485, # pars$par[1],
  soilm_par_a     = 0.33349283, # 0.59107163, # pars$par[2],
  soilm_par_b     = 1.45602286, # pars$par[3],
  tau_acclim_tempstress = 10,   # irrelevant - not calibrated
  par_shape_tempstress  = 0.0   # irrelevant - not calibrated
)

# run the model with the paramters ----
output <- runread_pmodel_f(
     drivers,
     par = params_modl,
     makecheck = TRUE,
     parallel = FALSE
     )

evalsites <- output %>%
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
  output,
  settings_eval,
  obs_eval = obs_eval,
  overwrite = TRUE,
  light = FALSE
  )

out_eval$gpp$fluxnet$plot$gg_modobs_xdaily
out_eval$gpp$fluxnet$plot$gg_modobs_spatial_annual
