# remotes::install_deps()
library(score4cast)
library(arrow)
ignore_sigpipe()


#readRenviron(path.expand("~/.Renviron"))
Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
Sys.unsetenv("AWS_DEFAULT_REGION")
options(mc.cores=4L)

endpoint = "data.ecoforecast.org"
s3_forecasts <- arrow::s3_bucket("tern4cast-forecasts", endpoint_override = endpoint)
s3_targets <- arrow::s3_bucket("tern4cast-targets", endpoint_override = endpoint)
## Publishing Requires AWS_ACCESS_KEY_ID & AWS_SECRET_ACCESS_KEY set
s3_scores <- arrow::s3_bucket("tern4cast-scores", endpoint_override = endpoint)
s3_prov <- arrow::s3_bucket("tern4cast-prov", endpoint_override = endpoint)

local_prov = paste0(theme, "-scoring-prov.csv")
s3_inv <- arrow::s3_bucket("tern4cast-inventory", endpoint_override = endpoint)


score4cast:::prov_download(s3_prov, local_prov)
prov_df <- readr::read_csv(local_prov, col_types = "cc")
on.exit(prov_upload(s3_prov, local_prov))

s3_scores_path <- s3_scores$path(glue::glue("parquet/{theme}", 
                                            theme = theme))
bucket <- "tern4cast-forecasts/"
timing <- bench::bench_time({
  target <- score4cast:::get_target(theme, s3_targets)
  grouping <- score4cast:::get_grouping(s3_inv, theme, collapse = TRUE, 
                           endpoint = endpoint)
  pb <- progress::progress_bar$new(format = glue::glue("  scoring {theme} [:bar] :percent in :elapsed, eta: :eta"), 
                                   total = nrow(grouping), clear = FALSE, width = 80)
  parallel::mclapply(seq_along(grouping[[1]]), score4cast:::score_group, 
                     grouping, bucket, target, prov_df, local_prov, s3_scores_path, 
                     pb, theme, endpoint)
})
prov_upload(s3_prov, local_prov)
timing


time <- score_theme("terrestrial_daily", s3_forecasts, s3_targets, s3_scores, s3_prov, 
                    s3_inv = arrow::s3_bucket("tern4cast-inventory", endpoint_override = endpoint))
message(paste("terrestrial_daily done in", time[["real"]]))