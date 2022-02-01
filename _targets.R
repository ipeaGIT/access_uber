options(
  java.parameters = "-Xmx100G",
  N_CORES = 30L
)
RcppParallel::setThreadOptions(numThreads = getOption("N_CORES"))

suppressPackageStartupMessages({
  library(targets)
  library(data.table)
  library(r5r)
  library(sf)
  library(dodgr)
})

source("R/01_calculate_matrix.R", encoding = "UTF-8")
source("R/misc.R", encoding = "UTF-8")

list(
  tar_target(
    uber_data,
    "../../data/access_uber/orig_ds_anonymized_v1.rds",
    format = "file"
  ),
  tar_target(
    pickup_data,
    "../../data-raw/uber_speed/pickup_anonymized.rds",
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
    grid_res_9,
    "../../data/acesso_oport/hex_agregados/2019/hex_agregado_rio_09_2019.rds",
    format = "file"
  ),
  tar_target(
    grid_res_8,
    "../../data/acesso_oport/hex_agregados/2019/hex_agregado_rio_08_2019.rds",
    format = "file"
  ),
  tar_target(
    rio_fare_integration,
    "../../data/access_uber/rio_fare_integration.csv",
    format = "file"
  ),
  tar_target(
    rio_routes_info,
    "../../data/access_uber/rio_routes_info.csv",
    format = "file"
  ),
  tar_target(travel_time_thresholds, c(30, 60, 90, 120)),
  tar_target(affordability_thresholds, seq(0, 0.6, by = 0.1)),
  tar_target(r5_points, generate_r5_points(grid_res_8), format = "file"),
  tar_target(
    pickup_data_res_8,
    aggregate_waiting_times(pickup_data, grid_res_8),
    format = "file"
  ),
  tar_target(
    full_uber_matrix,
    fill_uber_matrix(uber_data, pickup_data_res_8, grid_res_8),
    format = "file"
  ),
  tar_target(
    transit_pareto_frontier,
    calculate_transit_frontier(
      r5_points,
      graph_dir,
      rio_fare_integration,
      rio_routes_info  
    ),
    format = "file"
  ),
  tar_target(
    uber_first_mile_pareto_frontier,
    calculate_uber_first_mile_frontier(
      full_uber_matrix,
      rapid_transit_stations,
      graph_dir,
      r5_points,
      grid_res_8,
      rio_fare_integration,
      rio_routes_info
    ),
    format = "file"
  ),
  tar_target(
    uber_fm_transit_combined_frontier,
    join_uber_fm_transit_frontiers(
      uber_first_mile_pareto_frontier,
      transit_pareto_frontier
    ),
    format = "file"
  ),
  tar_target(
    all_frontiers,
    c(
      transit_pareto_frontier,
      uber_fm_transit_combined_frontier,
      full_uber_matrix
    )
  ),
  tar_target(
    affordability_frontiers,
    calculate_affordability(all_frontiers, grid_res_8),
    pattern = map(all_frontiers),
    format = "file"
  )
)