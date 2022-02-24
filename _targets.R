options(
  java.parameters = "-Xmx100G",
  N_CORES = 20L,
  INSIDE_PRIVATE_SERVER = FALSE
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
source("R/02_calculate_access.R", encoding = "UTF-8")
source("R/misc.R", encoding = "UTF-8")

# the transit pareto frontier target consumes a lot of resources and can be
# run inside our usual server, instead of the private server. so we build its
# target conditionally: if running in our usual server, generate the frontier as
# usual. if running inside the private server, just point to the previously
# calculate file

transit_frontier_target <- if (getOption("INSIDE_PRIVATE_SERVER") == TRUE) {
  tar_target(
    transit_pareto_frontier,
    "../../data/access_uber/pfrontiers/absolute/only_transit.rds",
    format = "file"
  )
} else {
  tar_target(
    transit_pareto_frontier,
    calculate_transit_frontier(
      r5_points,
      graph_dir,
      rio_fare_calculator
    ),
    format = "file"
  )
}

pipeline <- list(
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
    rio_fare_calculator,
    "../../data/access_uber/rio_fares_v3.zip",
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
    uber_first_mile_pareto_frontier,
    calculate_uber_first_mile_frontier(
      full_uber_matrix,
      rapid_transit_stations,
      graph_dir,
      r5_points,
      grid_res_8,
      rio_fare_calculator
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
    affordability_frontiers,
    calculate_affordability(
      full_uber_matrix,
      transit_pareto_frontier,
      uber_fm_transit_combined_frontier,
      grid_res_8
    ),
    format = "file"
  ),
  tar_target(
    accessibility,
    calculate_access(
      affordability_frontiers,
      travel_time_thresholds,
      affordability_thresholds,
      grid_res_8
    ),
    format = "file"
  ),
  tar_target(
    palma_ratio,
    calculate_palma(accessibility, grid_res_8),
    format = "file"
  )
)

list(pipeline, transit_frontier_target)
