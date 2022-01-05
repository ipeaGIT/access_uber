suppressPackageStartupMessages({
  library(targets)
})

list(
  tar_target(
    uber_data,
    "../../data-raw/uber_speed/orig_ds_anonymized.rds",
    format = "file"
  )
)