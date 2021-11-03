# Routine to format the benchmark driver dataset
# based upon FLUXNET data. This routine assumes
# that the code is run on th Euler compute
# infrastructure
#
# The data generated is meant to drive the p-model
# and provide a good working / test dataset.
# The routine used is largely based on previous work
# REFERENCE PAPER BENI.

# Libraries, check for Euler access ----
library(tidyverse)
library(rsofun)
library(ingestr)

if(!grepl('eu-', Sys.info()['nodename'])){
  stop("You are not on Euler, source data unavailable - abort abort abort!")
}

# Process driver data -----------------------------------------------------
message("-----------------------------------------------------")
message("Formatting driver data:")

# . set sites to ingest ----
fluxnet_sites <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(!(sitename %in% c("DE-Akm", "US-ORv", "DE-RuS")))
  #dplyr::filter(sitename == "FR-Pue") # for debugging

# . grab fluxnet data ----
message("- formatting fluxnet data")
df_fluxnet <-
  suppressWarnings(
    suppressMessages(
      ingestr::ingest(
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
    )
  )

# . get CRU data to complement fluxnet data ----
message("- formatting CRU data")
df_cru <- ingestr::ingest(
  siteinfo  = fluxnet_sites,
  source    = "cru",
  getvars   = "ccov",
  dir       = "~/data/cru/ts_4.01/"
  )

# . merge data into one "meteo" data frame ----
message("- combining meteo data")
df_meteo <- df_fluxnet %>%
  tidyr::unnest(data) %>%
  left_join(
    df_cru %>%
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>%
  group_by(sitename) %>%
  tidyr::nest()

# . grab MODIS FPAR data ----
message("- downloading MODIS fapar data")

settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = tempdir(),
  method_interpol   = "loess",
  network = c("fluxnet","icos"),
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  n_focal           = 0
  )

message("- downloading modis FAPAR")
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

# . grab CO2 data ----
message("- grabbing CO2 data")
df_co2 <- ingestr::ingest(
  fluxnet_sites,
  source  = "co2_mlo",
  verbose = FALSE
  )

# . set soil parameters ----
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

# . set simulation parameters ----
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

# . combine all data into the rsofun driver data format ----
message("-- combining all data into driver files")
p_model_fluxnet_drivers <- rsofun::collect_drivers_sofun(
  siteinfo       = fluxnet_sites,
  params_siml    = params_siml,
  meteo          = df_meteo,
  fapar          = df_modis_fpar,
  co2            = df_co2,
  df_soiltexture = df_soiltexture
  )

# . save data as a datafile, recognized by the package ----
message("-- saving data and DONE!")
save(p_model_fluxnet_drivers,
     file = "data/p_model_fluxnet_drivers.rda",
     compress = "xz")


# Prepare validation data -------------------------------------------------
message("-----------------------------------------------------")
message("Formating reference data:")

# load site selection New Phytologist
load("data/nphyt_sites.rda")

# create calibration selection
calib_sites <- fluxnet_sites %>%
  # excluded because fapar data could not be downloaded (WEIRD)
  dplyr::filter(!(sitename %in% c("DE-Akm", "IT-Ro1"))) %>%
  # excluded because no GPP data was found in FLUXNET file
  dplyr::filter(!(sitename %in% c("AU-Wom"))) %>%
  # excluded because some temperature data is missing
  dplyr::filter(sitename != "FI-Sod") %>%
  dplyr::filter( c4 %in% c(FALSE, NA) & classid != "CRO" & classid != "WET" ) %>%
  dplyr::filter( sitename %in% nphyt_sites ) %>%
  pull(sitename)

calib_sites <- fluxnet_sites %>%
  dplyr::filter(sitename %in% calib_sites)

# settings for data preparation
settings_ingestr_fluxnet <- list(
  dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/",
  getswc = FALSE,
  filter_ntdt = TRUE,
  threshold_GPP = 0.8,
  remove_neg = FALSE
)

# . format fluxnet GPP data ----

message(" - formatting ")

p_model_fluxnet_calval <-
  suppressWarnings(
    suppressMessages(
      ingestr::ingest(
        siteinfo = calib_sites,
        source    = "fluxnet",
        getvars = list(gpp = "GPP_NT_VUT_REF",
                       gpp_unc = "GPP_NT_VUT_SE"),
        dir = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
        settings = settings_ingestr_fluxnet,
        timescale = "d"
      )
    )
  )

message("-- saving data and DONE!")
save(p_model_fluxnet_calval,
     file = "data/p_model_fluxnet_calval.rda",
     compress = "xz")
