#' Call, build and run evaluation of the x latest rsofun versions
#'
#' Function needs no input for now.
#'
#' @param n_tags Integer. Number of tags to to be loaded.
#'
#' @return A nested list following the structure of eval_sofun(), plus two
#' additional plots for the seasonal cycles and the drought response.
#' @export

run_latest_versions <- function(
    n_tags  # number of tags to to be loaded
    ){

  # Define the GitHub API URL
  url <- "https://api.github.com/repos/geco-bern/rsofun/tags"

  # Make the API request
  response <- httr::GET(url)

  # Check if the request was successful (status code 200)
  if (grepl("OK", httr::http_status(response)$message)) {

    # Parse the JSON response
    tags_data <- jsonlite::fromJSON(url)

    # Extract the latest three releases
    latest_releases <- head(tags_data, n_tags)$name

    # Keep only releases above v4.0
    latest_releases <- latest_releases[grep("4", latest_releases)]

    # Print the latest releases
    # print(latest_releases)

  } else {
    # Handle the case where the request was not successful
    stop("API request failed with status code:", http_status(response)$message, "\n")
  }

  # Safety Check ----
  if (
    any(grepl("v3", latest_releases)) |
    any(grepl("v4.5", latest_releases))
    ) {
    stop(
      paste0(
        "\n--------------------------------------------------------------------------\n",
        "Function `run_latest_versions():\n",
        "Running latest versions has only been tested for v4.0 - v4.4.\n",
        "If a newer version (v4.4 or higher) should be added, make sure to format\n",
        "driver data accordingly, and adjust this error statement\n",
        "--------------------------------------------------------------------------\n"
        )
    )
  }

  # Prepare output list ----
  # Make list to contain all evaluation outputs
  all_versions <- list()

  # Loop Start ----
  for (version_nr in latest_releases) {

    # ## Install rsofun version ----
    # # if ("rsofun" %in% installed.packages()) remove.packages("rsofun")
    rsofun_version <- paste0("geco-bern/rsofun@", version_nr)
    devtools::install_github(rsofun_version,
                             upgrade = "never",
                             force = TRUE)

    library(rsofun)

    ## Clean console
    message("\014 Working on version: ", rsofun_version)

    ## Get input ----

    # only select the sites from the New Phytologist paper
    # load site list and filter on these (see below)
    flue_sites <- readr::read_csv(here::here("data/flue_stocker18nphyt.csv")) %>%
      dplyr::filter( !is.na(cluster) ) %>%
      distinct(site) %>%
      pull(site)

    # model forcing data
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

    # site selection: good-quality sites from analysis of Stocker et al. (2018) New Phyt.
    flue_sites <- readr::read_csv(here("data/flue_stocker18nphyt.csv")) %>%
      dplyr::filter( !is.na(cluster) ) %>%
      distinct(site) %>%
      pull(site)

    # observational data as calibration target
    # created from daily FLUXNE2015 data by:
    # - using GPP_NT_VUT_REF
    # - using QC>0.8
    ddf_fluxnet_gpp <- readr::read_rds(here("data/ddf_fluxnet_gpp_v4.2.rds"))

    # observational data for evaluation
    load(here("data/obs_eval_fluxnet2015.Rdata"))

    # site meta info
    siteinfo_fluxnet2015 <- df_drivers_fluxnet2015 |>
      select(sitename, site_info) |>
      unnest(site_info)

    # calibration sites
    calibsites <- siteinfo_fluxnet2015 %>%
      dplyr::filter(!(sitename %in% c("DE-Akm", "IT-Ro1"))) %>%  # excluded because fapar data could not be downloaded (WEIRD)
      # dplyr::filter(!(sitename %in% c("AU-Wom"))) %>%  # excluded because no GPP data was found in FLUXNET file
      dplyr::filter(sitename != "FI-Sod") %>%  # excluded because some temperature data is missing
      dplyr::filter( c4 %in% c(FALSE, NA) & classid != "CRO" & classid != "WET" ) %>%
      dplyr::filter( sitename %in% flue_sites ) %>%
      pull(sitename)

    # # BayesianTools calibration settings for v4.4
    # settings_calib <- list(
    #   method = "BayesianTools",
    #   metric = cost_likelihood_pmodel,
    #   control = list(
    #     sampler = "DEzs",
    #     settings = list(
    #       burnin = 1500,
    #       iterations = 12000,
    #       nrChains = 3,       # number of independent chains
    #       startValue = 3      # number of internal chains to be sampled
    #     )),
    #   par = list(
    #     kphio = list(lower = 0.03, upper = 0.15, init = 0.05),
    #     soilm_betao = list(lower = 0, upper = 1, init = 0.2),
    #     kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41),
    #     err_gpp = list(lower = 0.1, upper = 3, init = 0.8)
    #   )
    # )
    #
    # # calibrate model
    # set.seed(1982)
    # par_calib <- calib_sofun(
    #   drivers = df_drivers_fluxnet2015 |>
    #     dplyr::filter(sitename %in% calibsites) |>
    #     dplyr::mutate(forcing = purrr::map(forcing, ~rename(., rain = rainf, snow = snowf))),
    #   obs = ddf_fluxnet_gpp,
    #   settings = settings_calib,
    #   par_fixed = list(
    #     kphio_par_a = -0.0025,            # define model parameter values from
    #     kphio_par_b = 20,                 # Stocker et al. 2020
    #     soilm_thetastar    = 0.6*240,
    #     beta_unitcostratio = 146.0,
    #     rd_to_vcmax        = 0.014,
    #     tau_acclim         = 30.0),
    #   targets = "gpp"
    # )

    # GenSa calibration settings for v4.4
    settings_calib <- list(
      method = "GenSA",
      metric = cost_rmse_pmodel,
      control = list(maxit = 100),
      par = list(
        kphio = list(lower = 0.03, upper = 0.15, init = 0.05),
        soilm_betao = list(lower = 0, upper = 1, init = 0.2),
        kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41)
      )
    )

    # fixed (not calibrated) model parameters
    par_fixed = list(
      kphio_par_a = -0.0025,            # define model parameter values from
      kphio_par_b = 20,                 # Stocker et al. 2020
      soilm_thetastar    = 0.6*240,
      beta_unitcostratio = 146.0,
      rd_to_vcmax        = 0.014,
      tau_acclim         = 30.0
      )

    # calibrate model
    set.seed(1982)
    par_calib <- calib_sofun(
      drivers = df_drivers_fluxnet2015 |>
        dplyr::filter(sitename %in% calibsites) |>
        dplyr::mutate(forcing = purrr::map(forcing, ~rename(., rain = rainf, snow = snowf))) |>
        dplyr::mutate(forcing = purrr::map(forcing, ~mutate(., netrad = 0))),
      obs = ddf_fluxnet_gpp,
      settings = settings_calib,
      par_fixed = par_fixed,
      targets = "gpp",
      verbose = TRUE
    )

    params_modl <- list(
      kphio              = par_calib$par["kphio"],
      kphio_par_a        = par_fixed["kphio_par_a"],
      kphio_par_b        = par_fixed["kphio_par_b"],
      soilm_thetastar    = par_fixed["soilm_thetastar"],
      soilm_betao        = par_calib$par["soilm_betao"],
      beta_unitcostratio = par_fixed["beta_unitcostratio"],
      rd_to_vcmax        = par_fixed["rd_to_vcmax"],
      tau_acclim         = par_fixed["tau_acclim"],
      kc_jmax            = par_calib$par["kc_jmax"]
    )

    # run model
    output <- rsofun::runread_pmodel_f(
      df_drivers_fluxnet2015 |>
        dplyr::mutate(
          forcing = purrr::map(
            forcing,
            ~rename(., rain = rainf, snow = snowf))
          ) |>
        dplyr::mutate(
          forcing = purrr::map(
            forcing,
            ~mutate(., netrad = 0))
        ),
      par = params_modl
    )

    # evaluate outputs
    evalsites <- output %>%
      mutate(ntsteps = purrr::map_dbl(data, ~nrow(.))) %>%
      dplyr::filter(ntsteps > 0) %>%
      pull(sitename)

    settings_eval <- list(
      benchmark = list( gpp = c("fluxnet") ),
      sitenames = evalsites,
      agg       = 8
    )

    ##  Evaluate model ----
    out_eval <- eval_sofun(
      output,
      settings_eval,
      obs_eval = obs_eval,
      overwrite = TRUE,
      light = FALSE
    )

    ## Plot Seasonal Cycle ----
    p_sc <-
      out_eval$gpp$fluxnet$data$meandoydf_byclim %>%
      dplyr::filter(climatezone %in% c(
        "Aw south", "BSk north", "Cfa north",
        "Cfb north", "Cfb south", "Csa north",
        "Csb north", "Dfb north", "Dfc north")
      ) %>%
      dplyr::filter(koeppen_code != "-") %>%
      pivot_longer(
        c(obs_mean, mod_mean),
        names_to = "source",
        values_to = "gpp"
      ) %>%
      ggplot() +
      geom_ribbon(
        aes(x = doy, ymin = obs_min, ymax = obs_max),
        fill = "black",
        alpha = 0.2
      ) +
      geom_line(aes(x = doy, y = gpp, color = source), size = 0.4) +
      labs(y = expression( paste("Simulated GPP (g C m"^-2, " d"^-1, ")" ) ),
           x = "DOY") +
      facet_wrap( ~climatezone ) +
      theme_gray() +
      theme(legend.position = "bottom") +
      scale_color_manual(
        name = "Setup: ",
        values = c("red", "black")
      )

    ## Plot Drought Response ----
    source("../R/align_events.R")
    source("../R/eval_droughtresponse.R")

    df_dday_agg <- eval_droughtresponse(
      df = out_eval$gpp$fluxnet$data$ddf %>%
        rename(
          site = sitename
        ),
      path_flue = "../data/flue_stocker18nphyt.csv",
      before = 20,
      after = 105,
      leng_threshold = 10,
      nbins = 10,
      do_norm = TRUE
    )

    usecol = colorRampPalette( c("wheat3", "white") )( 5 )[2]

    p_dr <-
      df_dday_agg %>%
      ggplot() +
      geom_hline(
        yintercept = 0,
        color = "black",
        linetype = "dotted"
      ) +
      geom_vline(
        xintercept = 0,
        color = "black",
        linetype = "dotted"
      ) +
      geom_line(
        aes(
          x = dday,
          y = median
        ),
        size = 0.9) +
      geom_ribbon(
        aes(
          x = dday,
          ymin = q33,
          ymax = q66
        ),
        alpha = 0.3) +
      scale_color_manual(
        values = c(
          "BRC" = "black",
          "FULL" = "royalblue"
        ),
        name = "Setup"
      ) +
      scale_fill_manual(
        values = c(
          "BRC" = "black",
          "FULL" = "royalblue"
        ),
        name = "Setup") +
      ylim(-1.2, 2.2) +
      xlim(-20, 105) +
      scale_x_continuous(
        expand = c(0,0)
      ) +
      scale_y_continuous(
        expand = c(0,0)
      ) +
      labs(
        x = "Days after drought onset",
        y = expression( paste( "Bias (g C m"^{-1}, " d"^{-1}, ")"))
      ) +
      theme_classic()

    ## Attach plots to list
    all_versions[[version_nr]] <- out_eval
    all_versions[[version_nr]]$seasonal_cycle <- p_sc
    all_versions[[version_nr]]$drought_response <- p_dr

  }

  # Return data on all versions
  return(all_versions)
}
