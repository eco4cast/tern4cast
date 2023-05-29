print(paste0("Running Creating Terrestrial baselines at ", Sys.time()))

print(paste0("Running Creating Daily Terrestrial Forecasts at ", Sys.time()))

library(tidyverse)
library(lubridate)
library(aws.s3)
library(imputeTS)
library(tsibble)
library(fable)


#' set the random number for reproducible MCMC runs
set.seed(329)

config <- yaml::read_yaml("challenge_configuration.yaml")

#'Team name code
team_name <- "climatology"



targets <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/terrestrial_daily/terrestrial_daily-targets.csv.gz"), guess_max = 10000)


sites <- read_csv(config$site_table, show_col_types = FALSE) |>
  dplyr::filter(!is.na(data_url))

site_names <- sites$site_id

target_clim <- targets %>%
  mutate(doy = yday(datetime)) %>%
  group_by(doy, site_id, variable) %>%
  summarise(clim_mean = mean(observation, na.rm = TRUE),
            clim_sd = sd(observation, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(clim_mean = ifelse(is.nan(clim_mean), NA, clim_mean))

#curr_month <- month(Sys.Date())
curr_month <- month(Sys.Date())
if(curr_month < 10){
  curr_month <- paste0("0", curr_month)
}

curr_year <- year(Sys.Date())
start_date <- Sys.Date() + days(1)

forecast_dates <- seq(start_date, as_date(start_date + days(35)), "1 day")
forecast_doy <- yday(forecast_dates)

forecast_dates_df <- tibble(datetime = forecast_dates,
                            doy = forecast_doy)

forecast <- target_clim %>%
  mutate(doy = as.integer(doy)) %>%
  filter(doy %in% forecast_doy) %>%
  full_join(forecast_dates_df, by = 'doy') %>%
  arrange(site_id, datetime)

subseted_site_names <- unique(forecast$site_id)
site_vector <- NULL
for(i in 1:length(subseted_site_names)){
  site_vector <- c(site_vector, rep(subseted_site_names[i], length(forecast_dates)))
}

forecast_tibble1 <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                           site_id = site_vector,
                           variable = "nee")

forecast_tibble2 <- tibble(datetime = rep(forecast_dates, length(subseted_site_names)),
                           site_id = site_vector,
                           variable = "nee")

forecast_tibble <- bind_rows(forecast_tibble1, forecast_tibble2)

foreast <- right_join(forecast, forecast_tibble)

site_count <- forecast %>%
  select(datetime, site_id, variable, clim_mean, clim_sd) %>%
  filter(!is.na(clim_mean)) |>
  group_by(site_id, variable) %>%
  summarize(count = n(), .groups = "drop") |>
  filter(count > 2) |>
  distinct() |>
  pull(site_id)

combined <- forecast %>%
  filter(site_id %in% site_count) |>
  select(datetime, site_id, variable, clim_mean, clim_sd) %>%
  rename(mean = clim_mean,
         sd = clim_sd) %>%
  group_by(site_id, variable) %>%
  mutate(mu = imputeTS::na_interpolation(x = mean),
         sigma = median(sd, na.rm = TRUE))
combined <- combined %>%
  pivot_longer(c("mu", "sigma"),names_to = "parameter", values_to = "prediction") |>
  mutate(family = "normal") |>
  mutate(reference_datetime = min(combined$datetime) - lubridate::days(1),
         model_id = "climatology") |>
  select(model_id, datetime, reference_datetime, site_id, variable, family, parameter, prediction)

combined %>%
  filter(variable == "nee") |>
  pivot_wider(names_from = parameter, values_from = prediction) %>%
  ggplot(aes(x = datetime)) +
  geom_ribbon(aes(ymin=mu - sigma*1.96, ymax=mu + sigma*1.96), alpha = 0.1) +
  geom_point(aes(y = mu)) +
  facet_wrap(~site_id)

file_date <- combined$reference_datetime[1]

forecast_file <- paste("terrestrial_daily", file_date, "climatology.csv.gz", sep = "-")

write_csv(combined, forecast_file)

source("https://raw.githubusercontent.com/eco4cast/tern4cast/main/R/submit.R")

submit(forecast_file = forecast_file,
                  metadata = NULL,
                  ask = FALSE)

unlink(forecast_file)

source('R/fablePersistenceModelFunction.R')
# 1.Read in the targets data
# We are not doing a peristence for le right now.
targets <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/terrestrial_daily/terrestrial_daily-targets.csv.gz"), guess_max = 10000)
#targets <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/terrestrial_daily/terrestrial_daily-targets.csv.gz"), guess_max = 10000) %>%
#  filter(variable == 'nee')

# 2. Make the targets into a tsibble with explicit gaps
targets_ts <- targets %>%
  as_tsibble(key = c('variable', 'site_id'), index = 'datetime') %>%
  # add NA values up to today (index)
  fill_gaps(.end = Sys.Date())

# 3. Run through each via map
site_var_combinations <- expand.grid(site = unique(targets$site_id),
                                     var = unique(targets$variable)) %>%
  # assign the transformation depending on the variable. le is logged
  mutate(transformation = 'none') %>%
  mutate(boot_number = 200,
         h = 37,
         bootstrap = T,
         verbose = T)

# Runs the RW forecast for each combination of variable and site_id
RW_forecasts <- purrr::pmap_dfr(site_var_combinations, RW_daily_forecast)

# convert the output into EFI standard
RW_forecasts_EFI <- RW_forecasts %>%
  rename(parameter = .rep,
         prediction = .sim) %>%
  # For the EFI challenge we only want the forecast for future
  filter(datetime > Sys.Date()) %>%
  group_by(site_id, variable) %>%
  mutate(reference_datetime = min(datetime) - lubridate::days(1),
         family = "ensemble",
         model_id = "persistenceRW") %>%
  select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction)

#RW_forecasts_EFI |>
#  filter(site_id %in% unique(RW_forecasts_EFI$site_id)[1:24]) |>
#  ggplot(aes(x = time, y = prediction, group = ensemble)) +
#  geom_line() +
#  facet_wrap(~site_id)

# 4. Write forecast file
file_date <- RW_forecasts_EFI$reference_datetime[1]

forecast_file <- paste("terrestrial_daily", file_date, "persistenceRW.csv.gz", sep = "-")

write_csv(RW_forecasts_EFI, forecast_file)

source("https://raw.githubusercontent.com/eco4cast/tern4cast/main/R/submit.R")

submit(forecast_file = forecast_file,
                  metadata = NULL,
                 ask = FALSE)

unlink(forecast_file)

