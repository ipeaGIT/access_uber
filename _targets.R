options(
  java.parameters = "-Xmx100G",
  R5R_THREADS = 15
)

suppressPackageStartupMessages({
  library(targets)
  library(data.table)
  library(r5r)
})

list(
  tar_target(
    uber_data,
    "../../data-raw/uber_speed/orig_ds_anonymized.rds",
    format = "file"
  ),
  tar_target(
    rapid_transit_stations,
    "../../data-raw/uber_speed/rapid_transit_stations_city.csv",
    format = "file"
  )
)