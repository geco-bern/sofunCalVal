# This should generate something like in data/obs_eval_fluxnet2015.Rdata
# tmp: read as template
# load(here::here("data/obs_eval_fluxnet2015.Rdata"))

# ----------------
# obs_eval is a list, containing data frames aggregated to different levels:
# - annual: "adf"
# - monthly: "mdf"
# - x-daily: "xdf"
# - daily: "ddf"
#
# and a list element containing the breaks (dates) used for the x-daily

# each data frame is organised with sites along rows, has columns for multiple
# site meta info, and a column 'data' with time series nested.

# site meta info are:
# lon, lat, elv, classid, c4, whc, koeppen_code, igbp_land_use, plant_functional_type

# columns in data are:
# date, gpp, (gpp_qc, le, le_qc)
# ----------------

library(tidyverse)
library(FluxDataKit)

# get site meta information
siteinfo <- read_csv("~/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv")

# Read rsofun driver data, created with applying an additional filter in
# data-raw/00_prepare_fluxnet_driver_data.R
drivers <- read_rds(here::here("data/drivers.rds")) |>
  dplyr::select(-params_siml) |>
  tidyr::unnest(site_info) |>
  dplyr::rename(data = forcing) |>
  dplyr::mutate(data = purrr::map(data, ~dplyr::select(., date, gpp, gpp_qc, le, le_qc)))

# complement with additional site meta info and make consistent
drivers <- drivers |>
  left_join(
    siteinfo |>
      dplyr::select(sitename, koeppen_code, igbp_land_use),
    by = "sitename"
  ) |>
  dplyr::select(-whc, -canopy_height, -reference_height)

agg_monthly <- function(df){
  df |>
    mutate(
      year = year(date),
      month = month(date)
      ) |>
    group_by(year, month) |>
    summarise(
      gpp = mean(gpp),
      gpp_qc = mean(gpp_qc),
      le = mean(le),
      le_qc = mean(le_qc),
      .groups = "drop"
    )
}

agg_annual <- function(df){
  df |>
    mutate(
      year = year(date)
    ) |>
    group_by(year) |>
    summarise(
      gpp = mean(gpp),
      gpp_qc = mean(gpp_qc),
      le = mean(le),
      le_qc = mean(le_qc),
      .groups = "drop"
    )
}

agg_xdaily <- function(df, breaks){

  df |>
    mutate(bin = cut(
      date,
      breaks = breaks,
      include.lowest = TRUE,
      right = FALSE)
      ) |>
    group_by(bin) |>
    summarise(
      gpp = mean(gpp),
      gpp_qc = mean(gpp_qc),
      le = mean(le),
      le_qc = mean(le_qc),
      .groups = "drop"
    )
}

binwidth <- 8
breaks <- seq(
  from = lubridate::ymd("1990-01-01"),
  to = lubridate::ymd("2024-12-31"),
  by = paste0(binwidth, " days")
)

# construct object
obs_eval <- list(

  # daily - unchanged
  ddf = drivers,

  # monthly
  mdf = drivers |>
    mutate(data = purrr::map(data, ~agg_monthly(.))),

  # annual
  adf = drivers |>
    mutate(data = purrr::map(data, ~agg_annual(.))),

  # x-daily
  xdf = drivers |>
    mutate(data = purrr::map(data, ~agg_xdaily(., breaks = breaks))),

  # breaks of x-daily aggregation
  breaks = breaks
)

saveRDS(obs_eval, file = here::here("data/obs_eval.rds"))

