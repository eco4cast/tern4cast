#renv::restore()
readRenviron("~/.Renviron") # MUST come first

library(tidyverse)
library(score4cast)
library(arrow)

message(paste0("Starting Processing Submissions ", Sys.time()))

#remotes::install_deps()
challenge_config <- yaml::read_yaml("challenge_config.yml")
## A place to store everything
local_dir <- file.path(challenge_config$DATA_DIR, "submissions")
unlink(local_dir, recursive = TRUE)
fs::dir_create(local_dir)

# cannot  set region="" using environmental variables!!
region=challenge_config$AWS_DEFAULT_REGION
Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
           "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)

message("Downloading forecasts ...")

## Note: s3sync stupidly also requires auth credentials even to download from public bucket

#sink(tempfile()) # aws.s3 is crazy chatty and ignores suppressMessages()...
aws.s3::s3sync(local_dir, bucket= "neon4cast-submissions",  direction= "download", verbose = FALSE, region = region)
#sink()

submissions <- fs::dir_ls(local_dir, recurse = TRUE, type = "file")
# submissions_bucket <- list.files(local_dir, recursive = TRUE)
submissions_bucket <- basename(submissions)

themes <- names(challenge_config$themes)


if(length(submissions) > 0){
  
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")
  Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")
  s3 <- arrow::s3_bucket("neon4cast-forecasts", endpoint_override="data.ecoforecast.org")
  
  for(i in 1:length(submissions)){
    
    curr_submission <- basename(submissions[i])
    theme <-  stringr::str_split(curr_submission, "-")[[1]][1]
    submission_date <- lubridate::as_date(paste(stringr::str_split(curr_submission, "-")[[1]][2:4], 
                                                collapse = "-"))
    print(i)
    print(curr_submission)
    print(theme)
    
    example <- stringr::str_detect(curr_submission, pattern = "air2waterSat.csv.gz") | stringr::str_detect(curr_submission, pattern = "neon4cast-example")
    
    if((tools::file_ext(curr_submission) %in% c("nc", "gz", "csv", "xml")) & !is.na(submission_date) & !example){
      
      log_file <- paste0(local_dir, "/",curr_submission,".log")
      
      if(theme %in% themes){
        #if(theme %in% themes & submission_date <= Sys.Date()){
        
        capture.output({
          valid <- tryCatch(neon4cast::forecast_output_validator(file.path(local_dir,curr_submission)),
                            error = function(e) FALSE, 
                            finally = NULL)
        }, file = log_file, type = c("message"))
        
        if(valid){
          
          # pivot forecast before transferring
          if(!grepl("[.]xml", basename(submissions[i]))){
            fc <- read4cast::read_forecast(submissions[i])
            if("richness" %in% names(fc)){
            fc <- fc |> pivot_longer(cols = c("richness","abundance"), names_to = "variable", values_to = "predicted")
            }
            if("amblyomma_americanum" %in% names(fc)){
              fc <- fc |> pivot_longer(cols = c("amblyomma_americanum"), names_to = "variable", values_to = "predicted")
            }
            
            if(theme == "terrestrial_30min"){
              reference_datetime_format <- "%Y-%m-%d %H:%M:%S"
            }else{
              reference_datetime_format <- "%Y-%m-%d"
            }
            
            pubDate <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

            fc <- score4cast::standardize_forecast(fc,basename(submissions[i]), reference_datetime_format = reference_datetime_format)
            fc <- fc |> dplyr::mutate(date = lubridate::as_date(datetime),
                                      pubDate = pubDate)
            print(head(fc))
            path <- s3$path(paste0("parquet/", theme))
            fc |> write_dataset(path, format = 'parquet', 
                                partitioning=c("model_id", "reference_datetime", "date"))
            #unlink(tmp) 
          }
          
          Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
                     "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)
          
          aws.s3::copy_object(from_object = submissions_bucket[i], 
                              from_bucket = "neon4cast-submissions", 
                              to_object = paste0("raw/", theme,"/",basename(submissions[i])), 
                              to_bucket = "neon4cast-forecasts",
                              region=region)
          
          if(aws.s3::object_exists(object = paste0("raw/", theme,"/",basename(submissions[i])), bucket = "neon4cast-forecasts", region=region)){
            print("delete")
            aws.s3::delete_object(object = submissions_bucket[i], bucket = "neon4cast-submissions", region=region)
          }
        } else { 
          Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
                     "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)
          
          aws.s3::copy_object(from_object = submissions_bucket[i], 
                              to_object = paste0("not_in_standard/", basename(submissions[i])), 
                              from_bucket = "neon4cast-submissions", 
                              to_bucket = "neon4cast-forecasts", region=region)
          if(aws.s3::object_exists(object = paste0("not_in_standard/",basename(submissions[i])), bucket = "neon4cast-forecasts", region=region)){
            print("delete")
            aws.s3::delete_object(object = submissions_bucket[i], bucket = "neon4cast-submissions", region=region)
          }
          
          aws.s3::put_object(file = log_file, 
                             object = paste0("not_in_standard/", 
                                             basename(log_file)), 
                             bucket = "neon4cast-forecasts", region=region)
        }
      } else if(!(theme %in% themes)){
        Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
                   "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)
        
        aws.s3::copy_object(from_object = submissions_bucket[i], 
                            to_object = paste0("not_in_standard/",basename(submissions[i])), 
                            from_bucket = "neon4cast-submissions",
                            to_bucket = "neon4cast-forecasts", region=region)
        capture.output({
          message(basename(submissions[i]))
          message("incorrect theme name in filename")
          message("Options are: ", paste(themes, collapse = " "))
        }, file = log_file, type = c("message"))
        
        if(aws.s3::object_exists(object = paste0("not_in_standard/",basename(submissions[i])), bucket = "neon4cast-forecasts", region=region)){
          print("delete")
          Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
                     "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)
          aws.s3::delete_object(object = submissions_bucket[i],
                                bucket = "neon4cast-submissions", region=region)
        }
        
        Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
                   "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)
        
        aws.s3::put_object(file = log_file,
                           object = paste0("not_in_standard/", 
                                           basename(log_file)), 
                           bucket = "neon4cast-forecasts", region=region)
      }else{
        #Don't do anything because the date hasn't occur yet
      }
    }else{
      Sys.setenv("AWS_DEFAULT_REGION" = challenge_config$AWS_DEFAULT_REGION,
                 "AWS_S3_ENDPOINT" = challenge_config$AWS_S3_ENDPOINT)
      
      aws.s3::copy_object(from_object = submissions_bucket[i], 
                          to_object = paste0("not_in_standard/",basename(submissions[i])), 
                          from_bucket = "neon4cast-submissions",
                          to_bucket = "neon4cast-forecasts", region=region)
    }
  }
}
unlink(local_dir, recursive = TRUE)

message(paste0("Completed Processing Submissions ", Sys.time()))
