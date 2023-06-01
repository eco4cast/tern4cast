## setup
library(gdalcubes)
library(gefs4cast)

gdalcubes::gdalcubes_options(parallel=2*parallel::detectCores())
#gdalcubes::gdalcubes_options(parallel=TRUE)


sites <- readr::read_csv("tern_field_site_metadata.csv",
                    col_select = c("site_id", "latitude", "longitude"))

Sys.setenv("GEFS_VERSION"="v12")

dates <- seq(as.Date("2023-05-22"), Sys.Date()-1, by=1)

message("GEFS v12 stage1")
bench::bench_time({ # cirrus ~ 6days for full set
  s3 <- gefs_s3_dir(product = "stage1",
                    path = "tern4cast-drivers/noaa/",
                    gefs_version = Sys.getenv("GEFS_VERSION", "v12"),
                    endpoint = "https://sdsc.osn.xsede.org",
                    bucket = "bio230014-bucket01")
  have_dates <- gsub("reference_datetime=", "", s3$ls())
  missing_dates <- dates[!(as.character(dates) %in% have_dates)]
  gefs_to_parquet(missing_dates, path = s3, sites = sites)
})

dates <- seq(as.Date("2020-09-25"), Sys.Date()-1, by=1)

message("GEFS v12 pseudo")
bench::bench_time({ #32xlarge
  s3 <- gefs_s3_dir(product = "pseudo",
                    path = "tern4cast-drivers/noaa/",
                    gefs_version = Sys.getenv("GEFS_VERSION", "v12"),
                    endpoint = "https://sdsc.osn.xsede.org",
                    bucket = "bio230014-bucket01")
  have_dates <- gsub("reference_datetime=", "", s3$ls())
  missing_dates <- dates[!(as.character(dates) %in% have_dates)]
  
  print(missing_dates)
  gefs4cast:::gefs_pseudo_measures(missing_dates,  path = s3, sites = sites)
})

