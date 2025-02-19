
# in this R script, you can run the calibration
# !!! be sure to have the correct model version

library(rsofun)
library(readr)
library(dplyr)
library(tidyr)
library(here)

set.seed(42)

# training and testing function

train_test_division <- function(meta_info,test_precentage){

  # divide the driver in 4 catogries (based on the median value of temp and pet/p)
  group_1_2 <- meta_info[meta_info$mat < median(meta_info$mat),]

  group_1 <- group_1_2[group_1_2$p_over_pet < median(group_1_2$p_over_pet),]

  group_2 <- group_1_2[group_1_2$p_over_pet > median(group_1_2$p_over_pet),]

  group_3_4 <- meta_info[meta_info$mat > median(meta_info$mat),]

  group_3 <- group_3_4[group_3_4$p_over_pet < median(group_3_4$p_over_pet),]

  group_4 <- group_3_4[group_3_4$p_over_pet > median(group_3_4$p_over_pet),]

  # check that everything works
  #any(group_1$sitename %in% group_2$sitename %in% group_3$sitename %in% group_4$sitename)

  test_1 <- group_1 |> sample_n(ceiling(n()*test_precentage))

  test_2 <- group_2 |> sample_n(ceiling(n()*test_precentage))

  test_3 <- group_3 |> sample_n(ceiling(n()*test_precentage))

  test_4 <- group_4 |> sample_n(ceiling(n()*test_precentage))

  result <- rbind(test_1,test_2,test_3 ,test_4)$sitename

   return(result)
}

# using the 2015 driver data

load(here("data/df_drivers_fluxnet2015.Rdata"))

df_drivers_fluxnet2015 <- df_drivers_fluxnet2015 %>%
  dplyr::select(sitename, forcing) %>%
  unnest(forcing) %>%
  dplyr::filter(!(month(date)==2 & mday(date)==29)) %>%

  ## model requires flux per seconds now
  mutate(prec = prec / (60*60*24), ppfd = ppfd / (60*60*24)) %>%

  ## assuming all precipitation in liquid form
  mutate(rainf = prec, snowf = 0) %>%

  ## required for new version, but not used because
  mutate(tmin = temp, tmax = temp) %>%

  group_by(sitename) %>%
  nest() %>%
  rename(forcing = data) %>%
  right_join(
    df_drivers_fluxnet2015 %>%
      dplyr::select(-forcing),
    by = "sitename"
  ) %>%
  ungroup() %>%
  rename(site_info = siteinfo, params_soil = df_soiltexture)

load(here("data/obs_eval_fluxnet2015.Rdata"))

train_validation <- obs_eval$ddf|> select(sitename,data)

settings <- list(
  method              = "GenSA",
  metric              = cost_rmse_pmodel,
  control = list(
    maxit = 100),
  par = list(
    kphio = list(lower=0.03, upper=0.2, init = 0.05),
    kphio_par_a = list(lower = -0.0004, upper = 0.001, init = -0.0025),
    kphio_par_b = list(lower = 10, upper = 30, init = 20),
    soilm_thetastar = list(lower  = 0, upper = 240, init = 144)
  )
)

pars_old_data <- calib_sofun(
  drivers = train_driver |>
    dplyr::mutate(
      forcing = purrr::map(
        forcing,
        ~rename(., rain = rainf, snow = snowf))) |>
    dplyr::mutate(
      forcing = purrr::map(
        forcing,
        ~mutate(., netrad = NA))),
  obs = train_validation,
  settings = settings,
  targets = "gpp",
  par_fixed = list(
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
)

pars_old_data$sitename = train_sitename

write_rds(pars_old_data, here("data/calibration","param_old_data_calibrated.rds")
