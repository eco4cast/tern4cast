# remotes::install_deps()
library(score4cast)
library(arrow)
ignore_sigpipe()

source("R/ignore_sigpipes.R")
readRenviron(path.expand("~/.Renviron"))
Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
Sys.unsetenv("AWS_DEFAULT_REGION")
options(mc.cores=4L)

endpoint = "data.ecoforecast.org"
s3_forecasts <- arrow::s3_bucket("tern4cast-forecasts", endpoint_override = endpoint)
s3_targets <- arrow::s3_bucket("tern4cast-targets", endpoint_override = endpoint)
## Publishing Requires AWS_ACCESS_KEY_ID & AWS_SECRET_ACCESS_KEY set
s3_scores <- arrow::s3_bucket("tern4cast-scores", endpoint_override = endpoint)
s3_prov <- arrow::s3_bucket("tern4cast-prov", endpoint_override = endpoint)

time <- score_theme("terrestrial_daily", s3_forecasts, s3_targets, s3_scores, s3_prov)
message(paste("terrestrial_daily done in", time[["real"]]))

