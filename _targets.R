options(
  java.parameters = "-Xmx50G",
  N_CORES = 30L,
  SHOW_R5R_PROGRESS = TRUE
)
RcppParallel::setThreadOptions(numThreads = getOption("N_CORES"))

suppressPackageStartupMessages({
  library(targets)
  library(data.table)
  library(r5r)
  library(sf)
  library(dodgr)
  library(ggplot2)
  library(ggsn)
  library(gtfstools)
  library(cowplot)
  library(h3jsr)
  library(dplyr)
  library(future)
  library(furrr)
  library(testthat)
  library(quantreg)
})

source("R/01_calculate_matrix.R", encoding = "UTF-8")
source("R/02_calculate_access.R", encoding = "UTF-8")
source("R/03_compare_access.R", encoding = "UTF-8")
source("R/09_context_figures.R", encoding = "UTF-8")

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
    "../data/data-raw/aop_generated/rio_fares_v4.zip",
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
  tar_target(lang, c("en", "pt")),
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
    pickup_data_res_8_filled,
    fill_waiting_times(pickup_data_res_8, r5_points),
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
      pickup_data_res_8_filled,
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
    access_dist,
    create_dist_maps(
      adjusted_accessibility,
      grid_res_8,
      rio_city,
      rio_state,
      cost_type,
      travel_time_thresholds,
      monetary_thresholds,
      map_theme,
      lang
    ),
    pattern = cross(
      cross(map(adjusted_accessibility, cost_type, monetary_thresholds), lang),
      travel_time_thresholds
    ),
    format = "file"
  ),
  tar_target(
    diff_dist_map,
    create_single_diff_dist_map(
      adjusted_accessibility,
      grid_res_8,
      rio_city,
      rio_state,
      monetary_thresholds,
      map_theme,
      lang
    ),
    pattern = map(lang),
    format = "file"
  ),
  tar_target(
    diff_per_group_boxplot,
    create_diff_per_group_plot(
      adjusted_accessibility,
      grid_res_8,
      monetary_thresholds,
      line_chart_theme,
      lang
    ),
    pattern = map(lang),
    format = "file"
  ),
  tar_target(
    accessibility_heatmap,
    create_access_heatmap(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      monetary_thresholds,
      cost_type,
      lang
    ),
    pattern = cross(
      map(adjusted_accessibility, monetary_thresholds, cost_type),
      lang
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
      monetary_thresholds,
      cost_type,
      lang
    ),
    pattern = cross(
      map(adjusted_accessibility, cost_type, monetary_thresholds),
      lang
    ),
    format = "file"
  ),
  tar_target(
    avg_access_per_group,
    create_avg_access_per_group_plot(
      adjusted_accessibility,
      grid_res_8,
      line_chart_theme,
      travel_time_thresholds,
      monetary_thresholds,
      cost_type,
      lang
    ),
    pattern = cross(
      map(adjusted_accessibility, cost_type, monetary_thresholds),
      lang
    ),
    format = "file"
  ),
  tar_target(
    north,
    create_north(rio_city)
  ),
  tar_target(
    paper_context_figure,
    create_paper_context_figure(
      grid_res_9,
      grid_res_8,
      rio_city,
      rio_state,
      context_map_theme,
      north,
      graph_dir,
      lang
    ),
    pattern = map(lang),
    format = "file"
  )
)