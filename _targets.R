options(
  java.parameters = "-Xmx100G",
  R5R_THREADS = 15
)

suppressPackageStartupMessages({
  library(targets)
  library(data.table)
  library(r5r)
  library(sf)
})

source("R/01_calculate_matrix.R", encoding = "UTF-8")

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
  ),
  tar_target(
    graph_dir,
    "../../data/access_uber/r5/rio",
    format = "file"
  ),
  tar_target(
    grid,
    "../../data/acesso_oport/hex_agregados/2019/hex_agregado_rio_09_2019.rds",
    format = "file"
  ),
  tar_target(r5_points, generate_r5_points(grid), format = "file")
)