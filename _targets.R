options(
  java.parameters = "-Xmx50G",
  N_CORES = 30L,
  INSIDE_PRIVATE_SERVER = FALSE
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
    ifelse(
      getOption("INSIDE_PRIVATE_SERVER"),
      "../../data/access_uber/orig_ds.rds",
      "../../data/access_uber/orig_ds_anonymized_v1.rds"
    ),
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
  tar_target(rio_city, "../../data/access_uber/rio_city.rds", format = "file"),
  tar_target(
    rio_state,
    "../../data/access_uber/rio_state.rds",
    format = "file"
  ),
  tar_target(line_chart_theme, create_line_chart_theme()),
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
    transit_pareto_frontier,
    ifelse(
      getOption("INSIDE_PRIVATE_SERVER") == TRUE,
      "../../data/access_uber/pfrontiers/absolute/only_transit.rds",
      calculate_transit_frontier(
        r5_points,
        graph_dir,
        rio_fare_calculator
      )
    ),
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
      travel_time_thresholds
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
      travel_time_thresholds
    ),
    pattern = cross(map(accessibility, cost_type), travel_time_thresholds),
    format = "file"
  )
)