library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

load(here::here("data/obs_eval_fluxnet2015.Rdata"))

months <- obs_eval$ddf |>
  unnest(data) |>
  mutate(gpp = ifelse(gpp > 0,gpp,0)) |>
  group_by(sitename, year(date),month(date))|>
  summarise(gpp = sum(gpp, na.rm = TRUE))


days_not_na <- obs_eval$ddf |>
  unnest(data) |>
  filter(!is.na(gpp)) |>
  group_by(sitename, year(date),month(date))|>
  filter(!is.na(gpp)) |>
  summarise(n = n())

months <- left_join(months,days_not_na,by =c("sitename","year(date)","month(date)"))

months <- months |> arrange(`month(date)`)

# here I multiply gpp by the factor (days in month) / day not na

# so that the value of montlhy record are similar

month_norm <- NULL

number_day_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

for(i in 1:12){
  tmp <-months |> filter(`month(date)` == i)

  tmp <- tmp |> mutate(gpp = ifelse(is.na(n),gpp,gpp*(number_day_month[i]/n)))

  month_norm <- rbind(month_norm,tmp)
}

month_norm <- month_norm |>
  mutate(date = ymd(paste0(`year(date)`,"-",`month(date)`,"-01")))

month_norm <- data.frame(sitename = month_norm$sitename,
                         date = month_norm$date,
                         gpp =month_norm$gpp)

month_norm <- month_norm |>
  group_by(sitename) |>
  arrange(date)|>
  nest(data = c(date,gpp)) |>
  arrange(sitename)

mdf <- right_join(obs_eval$mdf |> select(-data),month_norm, by="sitename")

obs_eval$mdf <- mdf


# week
#
week <- obs_eval$ddf |>
  unnest(data) |>
  filter(!(month(date)==2 & mday(date)==29)) |>
  select(sitename,date,gpp)

weeks <- NULL

for(i in unique(week$sitename)){
  tmp = week |> filter(sitename == i)
  start_date <- tmp[1,]$date
  weeks_sequence <- start_date + weeks(0:ceiling(nrow(tmp)/7))

  weeks_sequence <- rep(weeks_sequence,each = 7)
  weeks_sequence <- weeks_sequence[1:nrow(tmp)]
  tmp$weeks <- weeks_sequence

  weeks <- rbind(weeks,tmp)
}

weeks <- weeks |>
  mutate(gpp = ifelse(gpp > 0,gpp,NA)) |>
  group_by(sitename, weeks)|>
  summarise(gpp = sum(gpp, na.rm = TRUE))


days_not_na <- weeks |>
  filter(!is.na(gpp)) |>
  group_by(sitename, weeks)|>
  filter(!is.na(gpp)) |>
  summarise(n = n())

weeks <- left_join(weeks,days_not_na,by =c("sitename","weeks"))

weeks <- weeks |>
  group_by(sitename) |>
  rename(inbin = weeks) |>
  mutate(inbin = factor(inbin)) |>
  nest(data = c(inbin,gpp)) |>
  arrange(sitename)

xdf <- right_join(obs_eval$xdf |> select(-data),weeks, by="sitename")

obs_eval$xdf <- xdf

write_rds(obs_eval, here::here("data","correct_2015_eval_data.rds"))
