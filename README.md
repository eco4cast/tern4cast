# EFI-TERN Ecological Forecasting Challenge

The following elements of a forecasting challenge workflow are included in this repo.  The GitHub Action is the automation configuration.

1) Generation of targets
  - R script: `targets/terrestrial_targets.R`
  - GitHub Action: `.github/workflows/targets.yaml`
3) Processing of submissions:
  - R script: `process_submissions.R`
  - GitHub Action: `.github/workflows/submissions.yaml`
4) Download of NOAA Global Ensemble Forecasting System for sites
  - R script: `drivers/gefs_download.R`
  - GitHub Action: `.github/workflows/gefs.yaml`
5) Generation of baseline forecasts
  - R script: `models/run_terrestrial_baselines.R`
  - GitHub Action: `.github/workflows/baselines.yaml`
6) Evaluation (scoring) of submitted forecasts:
  - R script: `scoring.R`
  - GitHub Action: `.github/workflows/scoring.yaml`
7) Generation of dashboard:
  - Quarto file: `dashboard/
  - GitHub Action: `.github/workflows/dashboard.yaml`
8) Generation of catalog:
  - R script: `stac/update_stac.R`
  - GitHub Action: `.github/workflows/catalog.yaml`



![workflow figure](https://esajournals.onlinelibrary.wiley.com/cms/asset/f66911d7-5e46-4d85-af39-43bd36b9a9bd/fee2616-fig-0001-m.jpg)
