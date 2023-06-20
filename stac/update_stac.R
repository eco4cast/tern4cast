library(jsonlite)
library(arrow)
library(dplyr)
library(lubridate)

# Forecasts

model_jsons <- fs::dir_ls("stac/terrestrial/forecasts/models/")

current_min <- NULL
current_max <- NULL

for(i in 1:length(model_jsons)){
  js <- read_json(model_jsons[i])

  df <- arrow::open_dataset(js$assets$parquet_items$href) |>
    summarize(min = min(datetime),
              max = max(datetime)) |>
    collect()

  js$properties$start_datetime <- df$min
  js$properties$end_datetime <- df$max

  current_max <- lubridate::as_date(max(current_max, lubridate::as_date(df$max)))
  current_min <- lubridate::as_date(min(current_min, lubridate::as_date(df$min)))

  write_json(x = js,
             path = model_jsons[i],
             pretty=TRUE,
             auto_unbox=TRUE)
}

forecast_json <- "stac/terrestrial/forecasts/collection.json"
js <- read_json(forecast_json)

js$extent$temporal$interval[[1]][[1]] <- paste0(current_min,'T00:00:00Z')
js$extent$temporal$interval[[1]][[2]] <- paste0(current_max,'T00:00:00Z')

write_json(x = js,
           path = forecast_json,
           pretty=TRUE,
           auto_unbox=TRUE)

# Scores

model_jsons <- fs::dir_ls("stac/terrestrial/scores/models/")

current_min <- NULL
current_max <- NULL

for(i in 1:length(model_jsons)){
  js <- read_json(model_jsons[i])

  df <- arrow::open_dataset(js$assets$parquet_items$href) |>
    filter(!is.na(crps)) |>
    summarize(min = min(datetime),
              max = max(datetime)) |>
    collect()

  js$properties$start_datetime <- df$min
  js$properties$end_datetime <- df$max

  current_max <- lubridate::as_date(max(current_max, lubridate::as_date(df$max)))
  current_min <- lubridate::as_date(min(current_min, lubridate::as_date(df$min)))

  write_json(x = js,
             path = model_jsons[i],
             pretty=TRUE,
             auto_unbox=TRUE)
}

forecast_json <- "stac/terrestrial/scores/collection.json"
js <- read_json(forecast_json)

js$extent$temporal$interval[[1]][[1]] <- paste0(current_min,'T00:00:00Z')
js$extent$temporal$interval[[1]][[2]] <- paste0(current_max,'T00:00:00Z')

write_json(x = js,
           path = forecast_json,
           pretty=TRUE,
           auto_unbox=TRUE)





