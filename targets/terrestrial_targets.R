
print(paste0("Running Creating Terrestrial Targets at ", Sys.time()))

library(tidyverse)
library(lubridate)
library(ncdf4)

config <- yaml::read_yaml("challenge_configuration.yaml")

sites <- read_csv(config$site_table, show_col_types = FALSE) |>
  filter(!is.na(data_url))

read_tern_site <- function(i, sites){
  nc <- ncdf4::nc_open(sites$data_url[i])

  co2 <- ncdf4::ncvar_get(nc, "Fco2")
  le <- ncdf4::ncvar_get(nc, "Fe")
  time <- ncdf4::ncvar_get(nc, "time")

  time <- lubridate::as_datetime("1800-01-01 00:00:00.0") + lubridate::seconds(time * 86400)

  co2 <- tibble::tibble(datetime = time,
                        site_id = sites$site_id[i],
                        variable = "nee",
                        observation = co2) |>
    mutate(observation = ifelse(observation < -100, NA, observation))

  le <- tibble::tibble(datetime = time,
                       site_id = sites$site_id[i],
                       variable = "le",
                       observation = le) |>
    mutate(observation = ifelse(observation < -100, NA, observation))

  bind_rows(co2, le)
}

tern_flux_target_30m <- map_dfr(1:nrow(sites), read_tern_site, sites)

full_time <- NULL

for(i in 1:nrow(sites)){

  site_targets <- tern_flux_target_30m |> filter(site_id == sites$site_id[i])

  full_time_vector <- seq(min(c(site_targets$datetime), na.rm = TRUE),
                          max(c(site_targets$datetime), na.rm = TRUE),
                          by = "30 min")

  df_nee <- tibble(datetime = full_time_vector,
                   site_id = rep(sites$site_id[i], length(full_time_vector)),
                   variable = "nee")
  df_le <- tibble(datetime = full_time_vector,
                  site_id = rep(sites$site_id[i], length(full_time_vector)),
                  variable = "le")
  full_time <- bind_rows(full_time, df_nee, df_le)

}

tern_flux_target_30m <- left_join(full_time, tern_flux_target_30m, by = c("datetime", "site_id", "variable"))

valid_dates <- tern_flux_target_30m %>%
  mutate(date = as_date(datetime)) %>%
  filter(!is.na(observation)) %>%
  group_by(date, site_id, variable) %>%
  summarise(count = n(), .groups = "drop")


tern_flux_target_daily <- tern_flux_target_30m %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date, site_id, variable) %>%
  summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
  left_join(valid_dates, by = c("date","site_id", "variable")) |>
  mutate(observation = ifelse(count > 24, observation, NA),
         observation = ifelse(is.nan(observation), NA, observation)) %>%
  rename(datetime = date) %>%
  select(-count) |>
  mutate(observation = ifelse(variable == "nee", (observation * 12 / 1000000) * (60 * 60 * 24), observation))

tern_flux_target_daily %>%
  filter(year(datetime) > 2021) %>%
  ggplot(aes(x = datetime, y = observation)) +
  geom_point() +
  facet_grid(variable~site_id, scale = "free")

ggplot(tern_flux_target_daily, aes(x = datetime, y = observation)) +
  geom_point() +
  facet_grid(variable~site_id, scales = "free")


flux_target_30m <- tern_flux_target_30m

flux_target_daily <- tern_flux_target_daily

s3 <- arrow::s3_bucket("tern4cast-targets/terrestrial_daily", endpoint_override = "data.ecoforecast.org")
arrow::write_csv_arrow(flux_target_daily, sink = s3$path("terrestrial_daily-targets.csv.gz"))

