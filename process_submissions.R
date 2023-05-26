library(tidyverse)
library(score4cast)
library(arrow)
library(glue)

source("https://raw.githubusercontent.com/eco4cast/tern4cast/main/R/forecast_output_validator.R")
config <- yaml::read_yaml("challenge_configuration.yaml")

AWS_DEFAULT_REGION <- stringr::str_split_fixed(config$endpoint,"\\.", 2)[,1]
region <- stringr::str_split_fixed(config$endpoint,"\\.", 2)[,1]
AWS_S3_ENDPOINT <- stringr::str_split_fixed(config$endpoint,"\\.", 2)[,2]
endpoint_override <- config$endpoint

message(paste0("Starting Processing Submissions ", Sys.time()))

local_dir <- file.path(here::here(), "submissions")
unlink(local_dir, recursive = TRUE)
fs::dir_create(local_dir)

# cannot  set region="" using environmental variables!!

Sys.setenv("AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION,
           "AWS_S3_ENDPOINT" = AWS_S3_ENDPOINT)

message("Downloading forecasts ...")

## Note: s3sync stupidly also requires auth credentials even to download from public bucket

aws.s3::s3sync(local_dir, bucket = config$submissions_bucket,  direction= "download", verbose = FALSE, region = region)

submissions <- fs::dir_ls(local_dir, recurse = TRUE, type = "file")
submissions_bucket <- basename(submissions)

themes <- config$themes

if(length(submissions) > 0){

  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")
  Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")
  s3 <- arrow::s3_bucket(config$forecasts_bucket, endpoint_override = endpoint_override)

  for(i in 1:length(submissions)){

    curr_submission <- basename(submissions[i])
    theme <-  stringr::str_split(curr_submission, "-")[[1]][1]
    submission_date <- lubridate::as_date(paste(stringr::str_split(curr_submission, "-")[[1]][2:4],
                                                collapse = "-"))

    print(curr_submission)
    print(theme)

    example <- stringr::str_detect(curr_submission, pattern = config$example_model_id)

    if((tools::file_ext(curr_submission) %in% c("nc", "gz", "csv")) & !is.na(submission_date) & !example){

      if(theme %in% themes){

        valid <- forecast_output_validator(file.path(local_dir, curr_submission))

        if(valid){

            fc <- read4cast::read_forecast(submissions[i])
            reference_datetime_format <- config$theme_datetime_format[which(themes == theme)]

            pubDate <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

            df <- mutate(fc, reference_datetime = strftime(lubridate::as_datetime(reference_datetime),
                                                           format = reference_datetime_format, tz = "UTC"))

            fc <- fc |> dplyr::mutate(date = lubridate::as_date(datetime),
                                      pubDate = pubDate)
            print(head(fc))
            path <- s3$path(paste0("parquet/", theme))
            fc |> write_dataset(path, format = 'parquet',
                                partitioning=c("model_id", "reference_datetime", "date"))

          Sys.setenv("AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION,
                     "AWS_S3_ENDPOINT" = AWS_S3_ENDPOINT)

          aws.s3::copy_object(from_object = submissions_bucket[i],
                              from_bucket = config$submissions_bucket,
                              to_object = paste0("raw/", theme,"/",basename(submissions[i])),
                              to_bucket = config$forecasts_bucket,
                              region=region)

          if(aws.s3::object_exists(object = paste0("raw/", theme,"/",basename(submissions[i])), bucket = config$forecasts_bucket, region = region)){

            aws.s3::delete_object(object = submissions_bucket[i], bucket = config$submissions_bucket, region=region)

          }
        } else {
          Sys.setenv("AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION,
                     "AWS_S3_ENDPOINT" = AWS_S3_ENDPOINT)

          aws.s3::copy_object(from_object = submissions_bucket[i],
                              to_object = paste0("not_in_standard/", basename(submissions[i])),
                              from_bucket = config$submissions_bucket,
                              to_bucket = config$forecasts_bucket, region=region)
          if(aws.s3::object_exists(object = paste0("not_in_standard/",basename(submissions[i])), bucket = config$forecasts_bucket, region = region)){

            aws.s3::delete_object(object = submissions_bucket[i], bucket = config$submissions_bucket, region=region)

          }
        }
      } else if(!(theme %in% themes)){
        Sys.setenv("AWS_DEFAULT_REGION" =AWS_DEFAULT_REGION,
                   "AWS_S3_ENDPOINT" = AWS_S3_ENDPOINT)

        aws.s3::copy_object(from_object = submissions_bucket[i],
                            to_object = paste0("not_in_standard/",basename(submissions[i])),
                            from_bucket = config$submissions_bucket,
                            to_bucket = config$forecasts_bucket, region=region)

        if(aws.s3::object_exists(object = paste0("not_in_standard/",basename(submissions[i])), bucket = config$forecasts_bucket, region = region)){


          Sys.setenv("AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION,
                     "AWS_S3_ENDPOINT" = AWS_S3_ENDPOINT)
          aws.s3::delete_object(object = submissions_bucket[i],
                                bucket = config$submissions_bucket, region = region)

        }
      }else{
        #Don't do anything because the date hasn't occur yet
      }
    }else{
      Sys.setenv("AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION,
                 "AWS_S3_ENDPOINT" = AWS_S3_ENDPOINT)

      aws.s3::copy_object(from_object = submissions_bucket[i],
                          to_object = paste0("not_in_standard/", basename(submissions[i])),
                          from_bucket = config$submissions_bucket,
                          to_bucket = config$forecasts_bucket, region = region)
    }
  }
}

unlink(local_dir, recursive = TRUE)

message(paste0("Completed Processing Submissions ", Sys.time()))
