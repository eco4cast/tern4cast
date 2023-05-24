
# remotes::install_github("cboettig/minio")
library(minio)
install_mc()
mc_alias_set("efi",  endpoint="data.ecoforecast.org",
             access_key = "", secret_key = "")

mc("mirror --overwrite efi/neon4cast-scores cache/")


