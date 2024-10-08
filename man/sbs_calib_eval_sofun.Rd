% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sbs_calib_eval_sofun.R
\name{sbs_calib_eval_sofun}
\alias{sbs_calib_eval_sofun}
\title{Conducts an site-by-site calibration}
\usage{
sbs_calib_eval_sofun(
  settings_calib,
  settings_eval,
  settings_sims,
  settings_input,
  df_drivers,
  ddf_obs_calib,
  obs_eval,
  overwrite
)
}
\arguments{
\item{settings_calib}{A list containing model calibration settings.
See vignette_rsofun.pdf for more information and examples.}

\item{settings_eval}{A list specifying evaluation settings
(see vignette eval_sofun.pdf for more information and examples)}

\item{settings_sims}{A list containing model simulation settings from
\code{\link{prepare_setup_sofun}}.  See vignette_rsofun.pdf for more
information and examples.}

\item{settings_input}{A list containing model input settings.
See vignette_rsofun.pdf for more information and examples.}

\item{df_drivers}{xxx}

\item{ddf_obs_calib}{A data frame containing observational data used for
model calibration. Created by function \code{get_obs_calib()}}

\item{obs_eval}{A data frame containing observational data used for model
evaluation Created by function \code{get_obs_eval()}}

\item{overwrite}{A boolean, defaults to \code{FALSE}.}
}
\value{
A nested list of objects returned by \code{\link{eval_sofun}}.
}
\description{
Wraps around calib_sofun() to calibrate site-by-site
(with one single site data for calibration and evaluation)
and returns the evaluation result.
}
