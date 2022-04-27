options(
  java.parameters = "-Xmx50G",
  N_CORES = 15L
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
source("R/09_context_figures.R", encoding = "UTF-8")
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
  tar_target(
    car_od_matrix,
    "../data/data-raw/aop_generated/OD_TI_carro_rio_06AM_distancias_H8.csv",
    format = "file"
  ),
  tar_target(line_chart_theme, create_line_chart_theme()),
  tar_target(map_theme, create_map_theme()),
  tar_target(context_map_theme, create_context_map_theme()),
  tar_target(travel_time_thresholds, c(30, 60, 90)),
  tar_target(
    monetary_thresholds,
    list(absolute = seq(0, 24, by = 6), affordability = seq(0, 0.4, by = 0.1))
  ),
  tar_target(cost_type, c("absolute", "affordability")),
  tar_target(r5_points, generate_r5_points(grid_res_8), format = "file"),
  tar_target(
    income_adjusted_grid,
    adjust_grid_income(grid_res_8),
    format = "file"
  ),
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
    calculate_transit_frontier(
      r5_points,
      graph_dir,
      rio_fare_calculator
    ),
    format = "file"
  ),
  tar_target(
    full_uber_matrix,
    fill_uber_matrix(
      uber_data,
      pickup_data_res_8,
      grid_res_8,
      car_od_matrix,
      r5_points
    ),
    format = "file"
  ),
  tar_target(
    walk_uber_frontier,
    calculate_walk_uber_frontier(full_uber_matrix, walk_only_matrix),
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
      walk_uber_frontier,
      transit_pareto_frontier,
      uber_fm_transit_combined_frontier,
      income_adjusted_grid
    ),
    format = "file"
  ),
  tar_target(
    problematic_hexagons,
    identify_problematic_hexs(graph_dir, r5_points)
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
    adjusted_accessibility,
    adjust_access(
      accessibility,
      cost_type,
      problematic_hexagons
    ),
    pattern = map(accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    palma,
    calculate_palma(adjusted_accessibility, grid_res_8, cost_type),
    pattern = map(adjusted_accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    access_dist,
    create_dist_maps(
      adjusted_accessibility,
      grid_res_8,
      rio_city,
      rio_state,
      cost_type,
      travel_time_thresholds,
      monetary_thresholds,
      map_theme
    ),
    pattern = cross(
      map(adjusted_accessibility, cost_type, monetary_thresholds),
      travel_time_thresholds
    ),
    format = "file"
  ),
  tar_target(
    avg_access,
    create_avg_access_plot(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      cost_type
    ),
    pattern = map(adjusted_accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    avg_access_per_group,
    create_avg_access_per_group_plot(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      cost_type
    ),
    pattern = map(adjusted_accessibility, cost_type),
    format = "file"
  ),
  tar_target(
    palma_plot,
    create_palma_plot(
      palma,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      monetary_thresholds,
      cost_type
    ),
    pattern = map(palma, cost_type, monetary_thresholds),
    format = "file"
  ),
  tar_target(
    diff_dist,
    create_diff_dist_maps(
      adjusted_accessibility,
      grid_res_8,
      rio_city,
      rio_state,
      cost_type,
      travel_time_thresholds,
      monetary_thresholds,
      map_theme
    ),
    pattern = cross(
      map(adjusted_accessibility, cost_type, monetary_thresholds),
      travel_time_thresholds
    ),
    format = "file"
  ),
  tar_target(
    avg_access_by_time,
    create_avg_access_by_time_plot(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      monetary_thresholds,
      cost_type
    ),
    pattern = map(adjusted_accessibility, cost_type, monetary_thresholds),
    format = "file"
  ),
  tar_target(
    avg_access_by_time_per_group,
    create_avg_access_by_time_per_group_plot(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      monetary_thresholds,
      cost_type
    ),
    pattern = map(adjusted_accessibility, cost_type, monetary_thresholds),
    format = "file"
  ),
  tar_target(
    transit_vs_uber_fm_comparison,
    create_comparison_scatter_plot(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      monetary_thresholds,
      cost_type
    ),
    pattern = cross(
      map(adjusted_accessibility, cost_type, monetary_thresholds),
      travel_time_thresholds
    ),
    format = "file"
  ),
  tar_target(
    north,
    create_north(rio_city)
  ),
  tar_target(
    pop_density_map,
    create_pop_density_map(
      grid_res_9,
      rio_city,
      rio_state,
      context_map_theme,
      north
    ),
    format = "file"
  )
)