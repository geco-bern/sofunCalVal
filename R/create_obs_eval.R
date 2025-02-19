create_obs_eval <- function(driver, fdk_site_info){

  obs_eval <- list()

  # daily data
  ddf_tibble_preparation <- driver |>
    unnest(forcing) |>
    mutate(le = le*(24*60*60)) |>
    group_by(sitename) |>
    select(date, gpp,le) |>
    nest()

  ddf_tibble_preparation <- ddf_tibble_preparation$data

  obs_eval$ddf <- tibble(sitename = fdk_site_info$sitename,
                         lon = fdk_site_info$lon,
                         lat = fdk_site_info$lat,
                         elv = fdk_site_info$elv,
                         classid = fdk_site_info$igbp_land_use,
                         whc = fdk_site_info$whc,
                         koeppen_code = fdk_site_info$koeppen_code,
                         igbp_land_use = fdk_site_info$igbp_land_use,
                         plant_functioanl_type= NA,
                         c4 = FALSE,
                         data = ddf_tibble_preparation)

  # weekly data
  xdf_tibble_preparation <- driver |>
    unnest(forcing) |>
    mutate(week = lubridate::floor_date(date, "week")) |>
    select(sitename, week, gpp,le) |>
    mutate(le = le*(24*60*60)) |>
    group_by(sitename, week) |>
    summarize(gpp = sum(gpp),
              le = sum(le)) |>
    mutate(inbin = as.factor(week), date = week) |>
    select(date, inbin, gpp,le) |>
    nest()

  xdf_tibble_preparation <- xdf_tibble_preparation$data

  obs_eval$xdf <- tibble(sitename = fdk_site_info$sitename,
                         lon = fdk_site_info$lon,
                         lat = fdk_site_info$lat,
                         elv = fdk_site_info$elv,
                         classid = fdk_site_info$igbp_land_use,
                         whc = fdk_site_info$whc,
                         koeppen_code = fdk_site_info$koeppen_code,
                         igbp_land_use = fdk_site_info$igbp_land_use,
                         plant_functioanl_type= NA,
                         c4 = FALSE,
                         data = xdf_tibble_preparation)

  # monthly data
  mdf_tibble_preparation <- driver |>
    unnest(forcing) |>
    mutate(month = lubridate::floor_date(date, "month")) |>
    select(sitename ,month,gpp,le) |>
    mutate(le = le*(24*60*60)) |>
    group_by(sitename, month) |>
    summarize(gpp = sum(gpp),
              le = sum(le)) |>
    mutate(date = month) |>
    select(date,gpp,le) |>
    nest()

  mdf_tibble_preparation <- mdf_tibble_preparation$data

  obs_eval$mdf <- tibble(sitename = fdk_site_info$sitename,
                         lon = fdk_site_info$lon,
                         lat = fdk_site_info$lat,
                         elv = fdk_site_info$elv,
                         classid = fdk_site_info$igbp_land_use,
                         whc = fdk_site_info$whc,
                         koeppen_code = fdk_site_info$koeppen_code,
                         igbp_land_use = fdk_site_info$igbp_land_use,
                         plant_functioanl_type= NA,
                         c4 = FALSE,
                         data = mdf_tibble_preparation)

  # annual data

  adf_tibble_preparation <- driver |>
    unnest(forcing) |>
    mutate(year = lubridate::floor_date(date, "year")) |>
    select(sitename ,year,gpp,le) |>
    mutate(le = le*(24*60*60)) |>
    group_by(sitename, year) |>
    summarize(gpp = sum(gpp),
              le = sum(le)) |>
    mutate(date = year) |>
    select(date,gpp,le) |>
    nest()

  adf_tibble_preparation <- adf_tibble_preparation$data

  obs_eval$adf <- tibble(sitename = fdk_site_info$sitename,
                         lon = fdk_site_info$lon,
                         lat = fdk_site_info$lat,
                         elv = fdk_site_info$elv,
                         classid = fdk_site_info$igbp_land_use,
                         whc = fdk_site_info$whc,
                         koeppen_code = fdk_site_info$koeppen_code,
                         igbp_land_use = fdk_site_info$igbp_land_use,
                         plant_functioanl_type= NA,
                         c4 = FALSE,
                         data = adf_tibble_preparation)

  breaks_preparation <- driver |>
    unnest(forcing) |>
    select(date) |>
    arrange(date) |>
    distinct(date) |>
    mutate(breaks = lubridate::floor_date(date, "week"))

  obs_eval$breaks <- unique((breaks_preparation$breaks))[-1]

  return(obs_eval)
}
