library(score4cast)
library(arrow)

ignore_sigpipe()

config <- yaml::read_yaml("challenge_configuration.yaml")

endpoint <- config$endpoint

bucket <- arrow::s3_bucket(config$forecasts_bucket,
                           endpoint_override = endpoint,
                           anonymous = TRUE)
inventory <- arrow::s3_bucket(config$inventory_bucket,
                              endpoint_override = endpoint,
                              access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                              secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
score4cast:::update_s3_inventory(bucket, inventory)

Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
Sys.unsetenv("AWS_DEFAULT_REGION")
options(mc.cores=4L)

s3_forecasts <- arrow::s3_bucket(config$forecasts_bucket, endpoint_override = endpoint)
s3_targets <- arrow::s3_bucket(config$targets_bucket, endpoint_override = endpoint)
s3_scores <- arrow::s3_bucket(config$scores_bucket, endpoint_override = endpoint)
s3_prov <- arrow::s3_bucket(config$prov_bucket, endpoint_override = endpoint)
s3_inv <- arrow::s3_bucket(config$inventory_bucket, endpoint_override = endpoint)

for(i in 1:length(config$themes)){
  message(paste("starting theme: ", config$themes[i]))
  time <- score_theme(config$themes[i], s3_forecasts, s3_targets,
                      s3_scores, s3_prov, s3_inv = s3_inv)
  message(paste(config$themes[i]," done in", time[["real"]]))
}
