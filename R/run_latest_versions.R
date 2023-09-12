#' Call, build and run evaluation of the x latest rsofun versions
#'
#' Function needs no input for now.
#'
#' @param none
#'
#' @return A nested list following the structure of eval_sofun(), plus two
#' additional plots for the seasonal cycles and the drought response.
#' @export
#'
#' @examples xxx
#'

run_latest_versions <- function(){

  # Get latest versions ----
  # Set number of tags to to be loaded
  n_tags <- 3

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
    print(latest_releases)

  } else {
    # Handle the case where the request was not successful
    stop("API request failed with status code:", http_status(response)$message, "\n")
  }

  # Prepare output list ----
  # Make list to contain all evaluation outputs
  all_versions <- list()

  # Loop Start ----
  for (version_nr in latest_releases) {

    ## Install rsofun version ----
    if ("rsofun" %in% installed.packages()) remove.packages("rsofun")
    rsofun_version <- paste0("computationales/rsofun@", version_nr)
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

    # load fluxnet driver data (prepared)
    load(here::here("data/df_drivers_fluxnet2015.rda"))

    # load observational data (prepared)
    load(here::here("data/obs_eval_fluxnet2015.rda"))

    # convert data to adhere to new p-model naming conventions
    drivers <- df_drivers_fluxnet2015 %>%
      dplyr::select(sitename, forcing) %>%
      unnest(forcing) %>%
      dplyr::filter(!(month(date) == 2 & mday(date) == 29)) %>%

      ## model requires flux per seconds now
      mutate(
        prec = prec / (60*60*24),
        ppfd = ppfd / (60*60*24),
        rain = prec,
        snow = 0,
        tmin = temp,
        tmax = temp
      ) %>%

      group_by(sitename) %>%
      nest() %>%
      rename(forcing = data) %>%
      right_join(
        df_drivers_fluxnet2015 %>%
          dplyr::select(-forcing),
        by = "sitename"
      ) %>%
      ungroup() %>%
      rename(
        site_info = siteinfo,
        params_soil = df_soiltexture
      )

    # only retain New Phytologist sites!
    drivers <- drivers %>%
      filter(sitename %in% flue_sites)

    # use a fixed model parameter set
    # assuming the same underlying mechanics
    # we'll not update these in new runs
    params_modl <- list(
      kphio           = 0.09423773,
      soilm_par_a     = 0.33349283,
      soilm_par_b     = 1.45602286,

      # Note, latter parameters were not calibrated
      tau_acclim_tempstress = 10,
      par_shape_tempstress  = 0.0
    )

    ##  Run model ----
    output <- rsofun::runread_pmodel_f(
      drivers,
      par = params_modl
    )

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
