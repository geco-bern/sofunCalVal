# Use rsofun driver data from FluxDataKit and filter to good-quality sequences

library(tidyverse)
library(FluxDataKit)

# Read rsofun driver data
p_model_fluxnet_drivers <- read_rds("~/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")

# Extract forcing time series
ddf <- p_model_fluxnet_drivers |>
  dplyr::select(-params_siml, -site_info) |>
  tidyr::unnest(forcing)

# Exclude croplands and wetlands
sites <- FluxDataKit::fdk_site_info |>
  dplyr::filter(!(igbp_land_use %in% c("CRO", "WET"))) |>

  # add information about good quality sequences
  dplyr::left_join(
    FluxDataKit::fdk_site_fullyearsequence,
    by = "sitename"
  ) |>
  dplyr::filter(!drop_gpp)

# filter data to retain only good quality sequences
ddf <- ddf |>
  dplyr::left_join(
    sites |>
      dplyr::select(
        sitename,
        year_start = year_start_gpp,
        year_end = year_end_gpp),
    by = join_by(sitename)
  ) |>
  dplyr::mutate(year = year(date)) |>
  dplyr::filter(year >= year_start & year <= year_end) |>
  dplyr::select(-year_start, -year_end, -year)

# Re-create rsofun driver data with clean data
drivers <- ddf |>
  dplyr::group_by(sitename) |>
  tidyr::nest() |>
  dplyr::rename(forcing = data) |>
  dplyr::left_join(
    p_model_fluxnet_drivers |>
      dplyr::select(sitename, params_siml, site_info),
    by = "sitename"
  ) |>
  dplyr::select(sitename, params_siml, site_info, forcing) |>
  dplyr::ungroup()

saveRDS(drivers, file = here::here("data/drivers.rds"))
