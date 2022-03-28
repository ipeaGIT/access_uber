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
  library(ggplot2)
})

source("R/01_calculate_matrix.R", encoding = "UTF-8")
source("R/02_calculate_access.R", encoding = "UTF-8")
source("R/03_compare_access.R", encoding = "UTF-8")
source("R/misc.R", encoding = "UTF-8")

list(
  tar_target(
    uber_data,
    "../data/data-raw/rio_origin_destination.csv",
    format = "file"
  ),
  tar_target(
    pickup_data,
    "../data/data-raw/rio_pickups.csv",
    format = "file"
  ),
  tar_target(
    rapid_transit_stations,
    "../data/data-raw/aop_generated/rapid_transit_stations_city.csv",
    format = "file"
  ),
  tar_target(
    graph_dir,
    "../r5/rio",
    format = "file"
  ),
  tar_target(
    grid_res_9,
    "../data/data-raw/aop_generated/hex_agregado_rio_09_2019.rds",
    format = "file"
  ),
  tar_target(
    grid_res_8,
    "../data/data-raw/aop_generated/hex_agregado_rio_08_2019.rds",
    format = "file"
  ),
  tar_target(
    rio_fare_calculator,
    "../data/data-raw/aop_generated/rio_fares_v3.zip",
    format = "file"
  ),
  tar_target(
    rio_city,
    "../data/data-raw/aop_generated/rio_city.rds",
    format = "file"
  ),
  tar_target(
    rio_state,
    "../data/data-raw/aop_generated/rio_state.rds",
    format = "file"
  ),
  tar_target(line_chart_theme, create_line_chart_theme()),
  tar_target(map_theme, create_map_theme()),
  tar_target(travel_time_thresholds, c(30, 60, 90, 120)),
  tar_target(
    monetary_thresholds,
    list(absolute = seq(0, 20, by = 5), affordability = seq(0, 0.6, by = 0.1))
  ),
  tar_target(cost_type, c("absolute", "affordability")),
  tar_target(r5_points, generate_r5_points(grid_res_8), format = "file"),
  tar_target(
    pickup_data_res_8,
    aggregate_waiting_times(pickup_data, grid_res_8),
    format = "file"
  ),
  tar_target(
    walk_only_matrix,
    calculate_walk_matrix(r5_points, graph_dir),
    format = "file"
  ),
  tar_target(
    transit_pareto_frontier,
    "../data/data-raw/aop_generated/only_transit_pareto_frontier.rds",
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
    frontiers_with_affordability,
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
      frontiers_with_affordability,
      travel_time_thresholds,
      monetary_thresholds,
      grid_res_8,
      r5_points,
      cost_type
    ),
    pattern = map(monetary_thresholds, cost_type),
    format = "file"
  ),
  tar_target(
    palma,
    calculate_palma(accessibility, grid_res_8, cost_type),
    pattern = map(accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    access_dist,
    create_dist_maps(
      accessibility,
      grid_res_8,
      rio_city,
      rio_state,
      cost_type,
      travel_time_thresholds,
      map_theme
    ),
    pattern = cross(map(accessibility, cost_type), travel_time_thresholds),
    format = "file"
  ),
  tar_target(
    avg_access,
    create_avg_access_plot(
      accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      cost_type
    ),
    pattern = map(accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    avg_access_per_group,
    create_avg_access_per_group_plot(
      accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      cost_type
    ),
    pattern = map(accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    palma_plot,
    create_palma_plot(
      palma,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      cost_type
    ),
    pattern = map(palma, cost_type),
    format = "file"
  ),
  tar_target(
    diff_dist,
    create_diff_dist_maps(
      accessibility,
      grid_res_8,
      rio_city,
      rio_state,
      cost_type,
      travel_time_thresholds,
      map_theme
    ),
    pattern = cross(map(accessibility, cost_type), travel_time_thresholds),
    format = "file"
  )
)