calibrate_rsofun <- function(use_drivers){

  # get calibration target data, filter only good-quality data to be used for
  # calibration
  get_validation_data <- function(df){
    df |>
      dplyr::filter(gpp_qc > 0.8) |>
      dplyr::select(date, gpp)
  }

  validation_data <- use_drivers |>
    select(sitename, data = forcing) |>
    mutate(data = purrr::map(
      data,
      ~get_validation_data(.)
      ))

  # Define calibration settings
  settings_calib <- list(
    method = "BayesianTools",
    metric = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 12000,
        iterations = 24000,
        nrChains = 3,       # number of independent chains
        startValue = 3      # number of internal chains to be sampled
      )),
    par = list(
      kphio = list(lower = 0.02, upper = 0.15, init = 0.05),
      kphio_par_a =list(lower = -0.004, upper = -0.001, init = -0.0025),
      kphio_par_b = list(lower = 10, upper = 30, init = 20),
      soilm_thetastar = list(
        lower = 0.01 * use_drivers$site_info[[1]]$whc,
        upper = 1.0  * use_drivers$site_info[[1]]$whc,
        init  = 0.6  * use_drivers$site_info[[1]]$whc
      ),
      soilm_betao = list(lower = 0.0, upper = 1.0, init = 0.0),
      err_gpp = list(lower = 0.1, upper = 3, init = 0.8)
    )
  )

  # Always hold the same parameters fixed
  par_calib <- calib_sofun(
    drivers = use_drivers,
    obs = validation_data,
    settings = settings_calib,
    par_fixed = list(
      beta_unitcostratio = 146.0,
      kc_jmax            = 0.41,
      rd_to_vcmax        = 0.014,
      tau_acclim         = 20.0
    ),
    targets = "gpp"
  )

  return(par_calib)
}
