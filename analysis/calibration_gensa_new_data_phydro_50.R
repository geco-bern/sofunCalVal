# if ("rsofun" %in% installed.packages()) remove.packages("rsofun")
# devtools::install_github(
#   "geco-bern/rsofun",
#   ref = "HEAD",
#   upgrade = "never",
#   force = TRUE
# )

library(rsofun)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ncdf4)

set.seed(42)

driver <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.rds") # currently only on workstation

# site meta info

fdk_site_info <- read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv")

fdk_filter <-  read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv")


fdk_site_info <- fdk_site_info[fdk_site_info$igbp_land_use != "CRO" &
                                 fdk_site_info$igbp_land_use != "WET"
                               # &
                               # fdk_site_info$sitename != "CA-SF1" &
                               # fdk_site_info$sitename != "CA-SF3" &
                               # fdk_site_info$sitename != "GF-Guy" &
                               # fdk_site_info$sitename != "ID-Pag" &
                               # fdk_site_info$sitename != "US-HWB" &
                               # fdk_site_info$sitename != "GL-Dsk"
                               ,]

fdk_filter <- fdk_filter[fdk_filter$drop_gpp == "FALSE",]

driver <-driver[which(driver$sitename %in% fdk_site_info$sitename &
                        driver$sitename %in% fdk_filter$sitename),]

fdk_site_info <-fdk_site_info[which(fdk_site_info$sitename %in% driver$sitename),]

fdk_filter <-fdk_filter[which(fdk_filter$sitename %in% driver$sitename &
                                fdk_filter$sitename %in% fdk_site_info$sitename),]

validation <- driver |>
  unnest(forcing) |>
  select(sitename,date,gpp) |>
  group_by(sitename) |>
  nest(data = c(date,gpp))

nc_file <- nc_open("~/data_scratch/sofuncalvalGPP/sofuncalvalGPP/data/whc_2m.nc")

whc = ncvar_get(nc_file, "whc_2m")
lons = ncvar_get(nc_file, "lon")
lats = ncvar_get(nc_file, "lat")

geo <- driver |>
  unnest(site_info) |>
  select(lon  , lat)

geo$sitename <- driver$sitename

n <- 1 # parameter to select size of slice to average

old_whc <- lapply(geo$sitename, function(x){
  tmp <- geo[geo$sitename == x,]
  lonid <- which(lons > tmp$lon)[1]
  latid <- which(lats > tmp$lat)[1]
  whc_grid <- whc[(lonid-n):(lonid+n), (latid-n):(latid+n)]
  whc_site <- mean(as.numeric(whc_grid, na.rm=T))
  return(whc_site)
})

old_whc = unlist(old_whc)

for(i in 1:dim(driver)[1]){
  driver$site_info[i][[1]][4] <- old_whc[i]
}

train_test_division <- function(meta_info,train_precentage){

  # divide the driver in 4 catogries (based on the median value of temp and pet/p)
  group_1_2 <- meta_info[meta_info$mat < median(meta_info$mat),]

  group_1 <- group_1_2[group_1_2$p_over_pet < median(group_1_2$p_over_pet),]

  group_2 <- group_1_2[group_1_2$p_over_pet > median(group_1_2$p_over_pet),]

  group_3_4 <- meta_info[meta_info$mat > median(meta_info$mat),]

  group_3 <- group_3_4[group_3_4$p_over_pet < median(group_3_4$p_over_pet),]

  group_4 <- group_3_4[group_3_4$p_over_pet > median(group_3_4$p_over_pet),]

  # check that everything works
  #any(group_1$sitename %in% group_2$sitename %in% group_3$sitename %in% group_4$sitename)

  test_1 <- group_1 |> sample_n(ceiling(n()*train_precentage))

  test_2 <- group_2 |> sample_n(ceiling(n()*train_precentage))

  test_3 <- group_3 |> sample_n(ceiling(n()*train_precentage))

  test_4 <- group_4 |> sample_n(ceiling(n()*train_precentage))

  result <- rbind(test_1,test_2,test_3 ,test_4)$sitename

  return(result)
}

train_sitename <- train_test_division(fdk_site_info,0.5)

train_driver <- driver[driver$sitename %in% train_sitename,]

train_validation <- validation[validation$sitename %in% train_sitename,]

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

pars_50 <- calib_sofun(
  drivers = train_driver,
  obs = train_validation,
  settings = settings,
  targets = "gpp",
  par_fixed = list(
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
)

pars_50$sitename <- train_sitename

write_rds(pars_50,"~/data_scratch/sofuncalvalGPP/sofuncalvalGPP/data/calibration/calibration_new_data_phydro_50.rds")






