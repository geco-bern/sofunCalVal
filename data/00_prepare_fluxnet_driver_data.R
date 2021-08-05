# Routine to format the benchmark driver dataset
# based upon FLUXNET data. This routine assumes
# that the code is run on th Euler compute
# infrastructure
#
# The data generated is meant to drive the p-model
# and provide a good working / test dataset.
# The routine used is largely based on previous work
# REFERENCE PAPER BENI.

# libraries, check for Euler access ----
library(tidyverse)
library(rsofun)
library(ingestr)

if(!grepl('eu-', Sys.info()['nodename'])){
  stop("You are not on Euler, source data unavailable - abort abort abort!")
}

# set sites to ingest ----
fluxnet_sites <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(!(sitename %in% c("DE-Akm", "US-ORv", "DE-RuS")))

# grab fluxnet data ----
df_fluxnet <- ingestr::ingest(
  siteinfo  = fluxnet_sites,
  source    = "fluxnet",
  getvars   = list(
    temp = "TA_F_DAY",
    prec = "P_F",
    vpd  = "VPD_F_DAY",
    ppfd = "SW_IN_F",
    patm = "PA_F"),
  dir       = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  settings  = list(
    dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/", getswc = FALSE),
  timescale = "d"
)

# get CRU data to complement fluxnet data ----
df_cru <- ingestr::ingest(
  siteinfo  = fluxnet_sites,
  source    = "cru",
  getvars   = "ccov",
  dir       = "~/data/cru/ts_4.01/"
  )

# merge data into one "meteo" data frame ----
df_meteo <- df_fluxnet %>%
  tidyr::unnest(data) %>%
  left_join(
    df_cru %>%
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>%
  group_by(sitename) %>%
  tidyr::nest()

# convert units and deal with missing data ----

# WHAT WITH TMIN TMAX RAIN ETC


# grab MODIS FPAR data ----
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_subsets/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal           = 0
  )

df_modis_fpar <- ingest(
  fluxnet_sites,
  source = "modis",
  settings = settings_modis,
  parallel = FALSE,
  ncores = 1
  )

df_modis_fpar <- df_modis_fpar %>%
  mutate(
    data = purrr::map(data, ~rename(., fapar = modisvar_filled))
    )

# grab CO2 data ----
df_co2 <- ingestr::ingest(
  fluxnet_sites,
  source  = "co2_mlo",
  verbose = FALSE
  )

# set soil parameters ----
df_soiltexture <- bind_rows(
  top    = tibble(
    layer = "top",
    fsand = 0.4,
    fclay = 0.3,
    forg = 0.1,
    fgravel = 0.1
    ),
  bottom = tibble(
    layer = "bottom",
    fsand = 0.4,
    fclay = 0.3,
    forg = 0.1,
    fgravel = 0.1)
)

# set simulation parameters ----
params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
	)

# combine all data into the rsofun driver data format ----
p_model_fluxnet_drivers <- rsofun::collect_drivers_sofun(
  siteinfo       = fluxnet_sites,
  params_siml    = params_siml,
  meteo          = df_meteo,
  fapar          = df_modis_fpar,
  co2            = df_co2,
  df_soiltexture = df_soiltexture
  )

# save data as a datafile, recognized by the package ----
save(p_model_fluxnet_drivers,
     file = "data/p_model_fluxnet_drivers.rda",
     compress = "xz")
